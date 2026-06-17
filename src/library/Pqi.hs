-- | A driver-agnostic reproduction of the ["postgresql-libpq"](https://hackage.haskell.org/package/postgresql-libpq) API (version
-- @0.11@, the pipelining-capable release).
--
-- The connection is reified as a single class @'IsConnection' c@ parameterised
-- over the connection type. Result accessors live in the independent
-- @'IsResult' r@ class, and cancellation handles in @'IsCancel' k@.
-- @'IsResult' ('ResultOf' c)@ and @'IsCancel' ('CancelOf' c)@ are superclass
-- constraints of @'IsConnection' c@, so callers who have access to a connection
-- get result inspection and cancellation automatically.
--
-- Function names, argument order, and semantics mirror the API of the C library binding
-- @postgresql-libpq@.
-- The only deliberate departures are:
--
-- * @Connection@, @Result@, and @Cancel@ become the class parameter @c@ and the
--   associated types @'ResultOf' c@ \/ @'CancelOf' c@.
--
-- * OIDs are a plain 'Word32' and row\/column\/parameter indices are a
--   plain 'Int32', rather than the C-specific newtypes of the original.
--
-- * Ambiguous, rarely-useful helpers (e.g. @resStatus@) are omitted, and the
--   inherently libpq-specific @libpqVersion@ lives in the FFI adapter instead
--   of this driver-agnostic interface.
--
-- * The 'unescapeBytea' helper is bundled in this library and implemented natively without IO.
module Pqi
  ( -- * Type classes
    IsConnection (..),
    IsResult (..),
    IsCancel (..),

    -- * Shared types
    Format (..),
    ExecStatus (..),
    ConnStatus (..),
    TransactionStatus (..),
    PollingStatus (..),
    PipelineStatus (..),
    FieldCode (..),
    Verbosity (..),
    FlushStatus (..),
    CopyInResult (..),
    CopyOutResult (..),
    LoFd (..),
    Notify (..),

    -- * Connection-independent helpers
    unescapeBytea,
  )
where

import Data.Bool
import Data.ByteString (ByteString)
import Data.Either
import Data.Eq
import Data.Int
import Data.Kind (Type)
import Data.Maybe
import Data.Ord
import Data.Word
import Pqi.UnescapeBytea (unescapeBytea)
import System.IO (FilePath, IO, IOMode, SeekMode)
import System.Posix.Types (Fd)
import Text.Show
import Prelude (Bounded, Enum)

-- * Shared types

-- | Format of a parameter or result column: textual or binary.
data Format
  = Text
  | Binary
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Status of a command result, as reported by @PQresultStatus@.
data ExecStatus
  = -- | The string sent to the server was empty.
    EmptyQuery
  | -- | Successful completion of a command returning no data.
    CommandOk
  | -- | Successful completion of a command returning data (such as a
    -- @SELECT@ or @SHOW@).
    TuplesOk
  | -- | Copy Out (from server) data transfer started.
    CopyOut
  | -- | Copy In (to server) data transfer started.
    CopyIn
  | -- | Copy In\/Out data transfer started.
    CopyBoth
  | -- | The server's response was not understood.
    BadResponse
  | -- | A nonfatal error (a notice or warning) occurred.
    NonfatalError
  | -- | A fatal error occurred.
    FatalError
  | -- | The @'ResultOf'@ contains a single result tuple from the current command.
    -- This status occurs only when single-row mode has been selected for the
    -- query.
    SingleTuple
  | -- | The @'ResultOf'@ represents a synchronization point in pipeline mode,
    -- requested by @'pipelineSync'@. This status occurs only in pipeline mode.
    PipelineSync
  | -- | The @'ResultOf'@ represents a pipeline that has received an error from
    -- the server. @'getResult'@ must be called repeatedly, and each time it will
    -- return this status code until the end of the current pipeline, at which
    -- point it will return @'PipelineSync'@ and normal processing can resume.
    PipelineAbort
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Status of a connection, as reported by @PQstatus@.
data ConnStatus
  = -- | The connection is ready.
    ConnectionOk
  | -- | The connection procedure has failed.
    ConnectionBad
  | -- | Waiting for connection to be made.
    ConnectionStarted
  | -- | Connection OK; waiting to send.
    ConnectionMade
  | -- | Waiting for a response from the server.
    ConnectionAwaitingResponse
  | -- | Received authentication; waiting for backend start-up to finish.
    ConnectionAuthOk
  | -- | Negotiating environment-driven parameter settings.
    ConnectionSetEnv
  | -- | Negotiating SSL encryption.
    ConnectionSSLStartup
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Current in-transaction status of the server, as reported by
-- @PQtransactionStatus@.
data TransactionStatus
  = -- | Currently idle.
    TransIdle
  | -- | A command is in progress.
    TransActive
  | -- | Idle, within a transaction block.
    TransInTrans
  | -- | Idle, within a failed transaction.
    TransInError
  | -- | Connection is bad.
    TransUnknown
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Result of a non-blocking connection-polling step.
data PollingStatus
  = PollingFailed
  | PollingReading
  | PollingWriting
  | PollingOk
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Pipeline-mode status of a connection, as reported by @PQpipelineStatus@.
data PipelineStatus
  = -- | The connection is in pipeline mode.
    PipelineOn
  | -- | The connection is /not/ in pipeline mode.
    PipelineOff
  | -- | The connection is in pipeline mode and an error occurred while
    -- processing the current pipeline.
    PipelineAborted
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Field identifier for the structured fields of an error report, as accepted
-- by @PQresultErrorField@.
data FieldCode
  = DiagSeverity
  | DiagSqlstate
  | DiagMessagePrimary
  | DiagMessageDetail
  | DiagMessageHint
  | DiagStatementPosition
  | DiagInternalPosition
  | DiagInternalQuery
  | DiagContext
  | DiagSourceFile
  | DiagSourceLine
  | DiagSourceFunction
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Verbosity of error reporting, as set by @PQsetErrorVerbosity@.
data Verbosity
  = ErrorsTerse
  | ErrorsDefault
  | ErrorsVerbose
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Result of attempting to flush the output buffer in non-blocking mode.
data FlushStatus
  = FlushOk
  | FlushFailed
  | FlushWriting
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Result of @PQputCopyData@\/@PQputCopyEnd@.
data CopyInResult
  = CopyInOk
  | CopyInError
  | CopyInWouldBlock
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Result of @PQgetCopyData@.
data CopyOutResult
  = CopyOutRow ByteString
  | CopyOutWouldBlock
  | CopyOutDone
  | CopyOutError
  deriving stock (Eq, Ord, Show)

-- | A large-object file descriptor, as returned by 'loOpen'.
newtype LoFd = LoFd Int32
  deriving stock (Eq, Ord, Show)

-- | An asynchronous notification, as returned by 'notifies'.
data Notify = Notify
  { relname :: ByteString,
    bePid :: Int32,
    extra :: ByteString
  }
  deriving stock (Eq, Ord, Show)

-- * Result inspection

-- | Result-accessor methods, independent of the connection type that produced
-- the result. This allows row decoders and projection functions (such as
-- 'observeResult' in @pqi-conformance@) to operate on any result type
-- without knowing the originating connection.
class IsResult r where
  -- | The status of the result.
  resultStatus :: r -> IO ExecStatus

  -- | The flat error message associated with the result, if any. Best-effort;
  -- see the note on 'errorMessage'.
  resultErrorMessage :: r -> IO (Maybe ByteString)

  -- | A single structured field of the result's error report.
  resultErrorField :: r -> FieldCode -> IO (Maybe ByteString)

  -- | Free the result. Adapters that manage results with the garbage collector
  -- may implement this as a no-op; for the C-backed adapter it frees the
  -- underlying @PGresult@, after which the result must not be used.
  unsafeFreeResult :: r -> IO ()

  -- | Number of rows (tuples) in the result.
  ntuples :: r -> IO Int32

  -- | Number of columns (fields) in the result.
  nfields :: r -> IO Int32

  -- | Name of the column at the given index.
  fname :: r -> Int32 -> IO (Maybe ByteString)

  -- | Index of the column with the given name, if present.
  fnumber :: r -> ByteString -> IO (Maybe Int32)

  -- | OID of the table the given column was fetched from, or 0.
  ftable :: r -> Int32 -> IO Word32

  -- | Column number (within its table) that the given result column was
  -- fetched from, or 0.
  ftablecol :: r -> Int32 -> IO Int32

  -- | Format (text or binary) of the given column.
  fformat :: r -> Int32 -> IO Format

  -- | Data type OID of the given column.
  ftype :: r -> Int32 -> IO Word32

  -- | Type modifier of the given column.
  fmod :: r -> Int32 -> IO Int

  -- | Server-side storage size of the given column's type, or a negative value
  -- for variable size.
  fsize :: r -> Int32 -> IO Int

  -- | Value at @(row, column)@, or @'Nothing'@ for SQL @NULL@. Delegates to
  -- @'getvalue''@ by default in case the adapter provides a copying variant.
  getvalue :: r -> Int32 -> Int32 -> IO (Maybe ByteString)
  getvalue = getvalue'

  -- | Like 'getvalue', but always returns a copy that remains valid after the
  -- result is freed.
  getvalue' :: r -> Int32 -> Int32 -> IO (Maybe ByteString)

  -- | Whether the value at @(row, column)@ is SQL @NULL@.
  getisnull :: r -> Int32 -> Int32 -> IO Bool

  -- | Length in bytes of the value at @(row, column)@.
  getlength :: r -> Int32 -> Int32 -> IO Int

  -- | Number of parameters of a prepared statement (for a 'describePrepared'
  -- result).
  nparams :: r -> IO Int32

  -- | Data type OID of the given prepared-statement parameter.
  paramtype :: r -> Int32 -> IO Word32

  -- | The command status tag of the result (e.g. @\"INSERT 0 1\"@).
  cmdStatus :: r -> IO (Maybe ByteString)

  -- | The number of rows affected by the command, as text.
  cmdTuples :: r -> IO (Maybe ByteString)

-- * Cancellation

-- | Cancellation of in-progress commands, isolated from the connection type.
class IsCancel k where
  -- | Request cancellation of the in-progress command via the handle.
  cancel :: k -> IO (Either ByteString ())

-- * Connection

-- | The single flat capability class: establishing, closing, inspecting,
-- querying, escaping, async commands, pipelining, cancellation handle
-- creation, notifications, copy, large objects, and control.
--
-- See the individual capability-class documentation (now inlined below) for
-- semantics of each method.
class (IsResult (ResultOf c), IsCancel (CancelOf c)) => IsConnection c where
  -- | The result type produced by this connection.
  type ResultOf c :: Type

  -- | The cancellation-handle type produced by this connection.
  type CancelOf c :: Type

  -- | Make a new (blocking) connection from a conninfo string.
  connectdb :: ByteString -> IO c

  -- | Begin establishing a connection asynchronously.
  connectStart :: ByteString -> IO c

  -- | Drive an asynchronous connection attempt forward.
  connectPoll :: c -> IO PollingStatus

  -- | A sentinel \"null\" connection.
  newNullConnection :: IO c

  -- | Whether a connection is the null sentinel.
  isNullConnection :: c -> Bool

  -- | Close the connection and release its resources.
  finish :: c -> IO ()

  -- | Reset the communication channel to the server (blocking).
  reset :: c -> IO ()

  -- | Begin resetting the connection asynchronously.
  resetStart :: c -> IO Bool

  -- | Drive an asynchronous reset forward.
  resetPoll :: c -> IO PollingStatus

  -- | The database name of the connection.
  db :: c -> IO (Maybe ByteString)

  -- | The user name of the connection.
  user :: c -> IO (Maybe ByteString)

  -- | The password of the connection.
  pass :: c -> IO (Maybe ByteString)

  -- | The server host name of the connection.
  host :: c -> IO (Maybe ByteString)

  -- | The port of the connection.
  port :: c -> IO (Maybe ByteString)

  -- | The command-line options passed in the connection request.
  options :: c -> IO (Maybe ByteString)

  -- | Current connection status.
  status :: c -> IO ConnStatus

  -- | Current in-transaction status of the server.
  transactionStatus :: c -> IO TransactionStatus

  -- | Look up a current parameter setting reported by the server.
  parameterStatus :: c -> ByteString -> IO (Maybe ByteString)

  -- | The frontend\/backend protocol version.
  protocolVersion :: c -> IO Int

  -- | The server version, as an integer of the form @MMmmpp@.
  serverVersion :: c -> IO Int

  -- | The most recent error message, if any.
  --
  -- Note: unlike the structured fields available via @'resultErrorField'@, the
  -- flat message text is formatted locally by the driver, so adapters are not
  -- expected to produce byte-identical strings.
  errorMessage :: c -> IO (Maybe ByteString)

  -- | The file descriptor of the connection socket.
  socket :: c -> IO (Maybe Fd)

  -- | The process ID of the backend serving this connection.
  backendPID :: c -> IO Int32

  -- | Whether the connection authentication method required a password but
  -- none was available.
  connectionNeedsPassword :: c -> IO Bool

  -- | Whether the connection authentication used a password.
  connectionUsedPassword :: c -> IO Bool

  -- | Submit a command and wait for the result.
  exec :: c -> ByteString -> IO (Maybe (ResultOf c))

  -- | Submit a parameterized command. Each parameter is given as
  -- @(type oid, value, format)@, or @'Nothing'@ for SQL @NULL@. The final
  -- @'Format'@ selects the result format.
  execParams ::
    c ->
    ByteString ->
    [Maybe (Word32, ByteString, Format)] ->
    Format ->
    IO (Maybe (ResultOf c))

  -- | Prepare a named statement. The OID list, when supplied, fixes parameter
  -- types; @'Nothing'@ leaves them to be inferred.
  prepare :: c -> ByteString -> ByteString -> Maybe [Word32] -> IO (Maybe (ResultOf c))

  -- | Execute a previously prepared statement. Each parameter is
  -- @(value, format)@, or @'Nothing'@ for SQL @NULL@.
  execPrepared ::
    c ->
    ByteString ->
    [Maybe (ByteString, Format)] ->
    Format ->
    IO (Maybe (ResultOf c))

  -- | Describe a prepared statement.
  describePrepared :: c -> ByteString -> IO (Maybe (ResultOf c))

  -- | Describe a portal.
  describePortal :: c -> ByteString -> IO (Maybe (ResultOf c))

  -- | Escape a string for safe inclusion in an SQL literal.
  escapeStringConn :: c -> ByteString -> IO (Maybe ByteString)

  -- | Escape binary data for use within a @bytea@ literal.
  escapeByteaConn :: c -> ByteString -> IO (Maybe ByteString)

  -- | Escape a string for use as an SQL identifier (e.g. a table or column
  -- name), including the surrounding double quotes.
  escapeIdentifier :: c -> ByteString -> IO (Maybe ByteString)

  -- | Submit a command without waiting for the result.
  sendQuery :: c -> ByteString -> IO Bool

  -- | Asynchronous @'execParams'@.
  sendQueryParams :: c -> ByteString -> [Maybe (Word32, ByteString, Format)] -> Format -> IO Bool

  -- | Asynchronous @'prepare'@.
  sendPrepare :: c -> ByteString -> ByteString -> Maybe [Word32] -> IO Bool

  -- | Asynchronous @'execPrepared'@.
  sendQueryPrepared :: c -> ByteString -> [Maybe (ByteString, Format)] -> Format -> IO Bool

  -- | Asynchronous @'describePrepared'@.
  sendDescribePrepared :: c -> ByteString -> IO Bool

  -- | Asynchronous @'describePortal'@.
  sendDescribePortal :: c -> ByteString -> IO Bool

  -- | Collect the next result from an asynchronous command.
  getResult :: c -> IO (Maybe (ResultOf c))

  -- | Read input from the server into the driver's buffer.
  consumeInput :: c -> IO Bool

  -- | Whether a command is busy (a @'getResult'@ would block).
  isBusy :: c -> IO Bool

  -- | Set the non-blocking flag of the connection.
  setnonblocking :: c -> Bool -> IO Bool

  -- | Whether the connection is in non-blocking mode.
  isnonblocking :: c -> IO Bool

  -- | Select single-row mode for the currently executing query.
  setSingleRowMode :: c -> IO Bool

  -- | Flush queued output data to the server.
  flush :: c -> IO FlushStatus

  -- | Current pipeline-mode status.
  pipelineStatus :: c -> IO PipelineStatus

  -- | Enter pipeline mode.
  enterPipelineMode :: c -> IO Bool

  -- | Leave pipeline mode.
  exitPipelineMode :: c -> IO Bool

  -- | Mark a synchronization point in a pipeline.
  pipelineSync :: c -> IO Bool

  -- | Request the server to flush its output buffer in pipeline mode.
  sendFlushRequest :: c -> IO Bool

  -- | Obtain a cancellation handle for the connection.
  getCancel :: c -> IO (Maybe (CancelOf c))

  -- | Return the next notification from the queue, if any.
  notifies :: c -> IO (Maybe Notify)

  -- | Stop accumulating notices for retrieval via @'getNotice'@.
  disableNoticeReporting :: c -> IO ()

  -- | Start accumulating notices for retrieval via @'getNotice'@.
  enableNoticeReporting :: c -> IO ()

  -- | Retrieve the next accumulated notice, if any.
  getNotice :: c -> IO (Maybe ByteString)

  -- | Send data on a @COPY FROM STDIN@ connection.
  putCopyData :: c -> ByteString -> IO CopyInResult

  -- | Signal the end of @COPY FROM STDIN@; @'Just'@ aborts with the given error.
  putCopyEnd :: c -> Maybe ByteString -> IO CopyInResult

  -- | Receive data on a @COPY TO STDOUT@ connection. The @'Bool'@ selects
  -- non-blocking mode.
  getCopyData :: c -> Bool -> IO CopyOutResult

  -- | Create a new large object.
  loCreat :: c -> IO (Maybe Word32)

  -- | Create a new large object with the given OID.
  loCreate :: c -> Word32 -> IO (Maybe Word32)

  -- | Import a file as a new large object.
  loImport :: c -> FilePath -> IO (Maybe Word32)

  -- | Import a file as a new large object with the given OID.
  loImportWithOid :: c -> FilePath -> Word32 -> IO (Maybe Word32)

  -- | Export a large object to a file.
  loExport :: c -> Word32 -> FilePath -> IO (Maybe ())

  -- | Open a large object.
  loOpen :: c -> Word32 -> IOMode -> IO (Maybe LoFd)

  -- | Write to an open large object.
  loWrite :: c -> LoFd -> ByteString -> IO (Maybe Int)

  -- | Read from an open large object.
  loRead :: c -> LoFd -> Int -> IO (Maybe ByteString)

  -- | Seek within an open large object.
  loSeek :: c -> LoFd -> SeekMode -> Int -> IO (Maybe Int)

  -- | Report the current seek position of an open large object.
  loTell :: c -> LoFd -> IO (Maybe Int)

  -- | Truncate an open large object.
  loTruncate :: c -> LoFd -> Int -> IO (Maybe ())

  -- | Close an open large object.
  loClose :: c -> LoFd -> IO (Maybe ())

  -- | Remove a large object.
  loUnlink :: c -> Word32 -> IO (Maybe ())

  -- | The current client encoding name.
  clientEncoding :: c -> IO ByteString

  -- | Set the client encoding.
  setClientEncoding :: c -> ByteString -> IO Bool

  -- | Set error verbosity, returning the previous setting.
  setErrorVerbosity :: c -> Verbosity -> IO Verbosity

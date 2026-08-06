-- | A driver-agnostic reproduction of the [@postgresql-libpq@](https://hackage.haskell.org/package/postgresql-libpq) @0.11@ API
-- (the pipelining-capable release).
--
-- t'Connection', t'Result' and t'Cancel' are records of @IO@ closures, each
-- already closed over the handle it needs (a C @PGconn@ pointer, a native
-- socket, ...).
--
-- Connections come from adapter packages - @pqi-ffi@ (C @libpq@ via
-- @postgresql-libpq@) and @pqi-native@ (pure-Haskell wire protocol) - each
-- exporting one top-level t'Adapter' value. An adapter must be identical
-- to @libpq@ on every protocol-derived value. @pqi-conformance@ enforces that
-- differentially.
--
-- == Differences from @postgresql-libpq@
--
-- Everything else - names, argument order, semantics - mirrors
-- @Database.PostgreSQL.LibPQ@.
--
-- * Connection acquisition lives in t'Adapter': @connectdb@, @connectStart@
--   and @newNullConnection@ are its fields, not top-level functions.
--
-- * @unescapeBytea@ and @resStatus@ are t'Adapter' fields too. They take no
--   connection, but their implementations are adapter-specific.
--
-- * t'Connection', t'Result' and t'Cancel' are records of closures rather than
--   opaque handles fed to top-level functions. Call sites are unchanged:
--   @exec connection sql@ selects a field and applies it.
--
-- * @Oid@ is 'Word32'; @Row@, @Column@, @LoFd@ are 'Int32'. No @invalidOid@
--   constant - it is @0@.
--
-- * @libpqVersion@ is omitted.
module Pqi
  ( -- * Adapter
    Adapter (..),

    -- * Connection
    Connection (..),

    -- * Result inspection
    Result (..),

    -- * Cancellation
    Cancel (..),

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
    Notify (..),
  )
where

import Data.Bool
import Data.ByteString (ByteString)
import Data.Either
import Data.Eq
import Data.Int
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import Data.Word
import Prelude (Bounded, Enum)
import System.IO (FilePath, IO, IOMode, SeekMode)
import System.Posix.Types (Fd)
import Text.Show

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
  | -- | The t'Result' contains a single result tuple from the current command.
    -- This status occurs only when single-row mode has been selected for the
    -- query.
    SingleTuple
  | -- | The t'Result' represents a synchronization point in pipeline mode,
    -- requested by 'pipelineSync'. This status occurs only in pipeline mode.
    PipelineSync
  | -- | The t'Result' represents a pipeline that has received an error from
    -- the server. 'getResult' must be called repeatedly, and each time it will
    -- return this status code until the end of the current pipeline, at which
    -- point it will return 'PipelineSync' and normal processing can resume.
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

-- | An asynchronous notification, as returned by 'notifies'.
data Notify = Notify
  { notifyRelname :: ByteString,
    notifyBePid :: Int32,
    notifyExtra :: ByteString
  }
  deriving stock (Eq, Ord, Show)

-- * Result inspection

-- | Result-accessor closures, closed over an adapter's own result
-- representation (e.g. a C @PGresult@ pointer).
--
-- Carries no reference to the connection that produced it, so decoders can
-- consume a result without knowing its origin.
data Result = Result
  { -- | The status of the result.
    resultStatus :: IO ExecStatus,
    -- | The flat error message of the result, if any. Formatted locally by the
    -- driver, so adapters are not expected to agree byte for byte; use
    -- 'resultErrorField' where exactness matters.
    resultErrorMessage :: IO (Maybe ByteString),
    -- | A single structured field of the result's error report.
    resultErrorField :: FieldCode -> IO (Maybe ByteString),
    -- | Free the result, after which it must not be used. A no-op in adapters
    -- that leave results to the garbage collector.
    unsafeFreeResult :: IO (),
    -- | Number of rows (tuples) in the result.
    ntuples :: IO Int32,
    -- | Number of columns (fields) in the result.
    nfields :: IO Int32,
    -- | Name of the column at the given index.
    fname :: Int32 -> IO (Maybe ByteString),
    -- | Index of the column with the given name, if present.
    fnumber :: ByteString -> IO (Maybe Int32),
    -- | OID of the table the given column was fetched from, or 0.
    ftable :: Int32 -> IO Word32,
    -- | Column number (within its table) that the given result column was
    -- fetched from, or 0.
    ftablecol :: Int32 -> IO Int32,
    -- | Format (text or binary) of the given column.
    fformat :: Int32 -> IO Format,
    -- | Data type OID of the given column.
    ftype :: Int32 -> IO Word32,
    -- | Type modifier of the given column.
    fmod :: Int32 -> IO Int,
    -- | Server-side storage size of the given column's type, or a negative value
    -- for variable size.
    fsize :: Int32 -> IO Int,
    -- | Value at @(row, column)@, or 'Nothing' for SQL @NULL@. May alias the
    -- result's storage, which 'unsafeFreeResult' invalidates.
    getvalue :: Int32 -> Int32 -> IO (Maybe ByteString),
    -- | Like 'getvalue', but always a copy, valid after the result is freed.
    getvalue' :: Int32 -> Int32 -> IO (Maybe ByteString),
    -- | Whether the value at @(row, column)@ is SQL @NULL@.
    getisnull :: Int32 -> Int32 -> IO Bool,
    -- | Length in bytes of the value at @(row, column)@.
    getlength :: Int32 -> Int32 -> IO Int,
    -- | Number of parameters of a prepared statement (for a 'describePrepared'
    -- result).
    nparams :: IO Int32,
    -- | Data type OID of the given prepared-statement parameter.
    paramtype :: Int32 -> IO Word32,
    -- | The command status tag of the result (e.g. @\"INSERT 0 1\"@).
    cmdStatus :: IO (Maybe ByteString),
    -- | The number of rows affected by the command, as text.
    cmdTuples :: IO (Maybe ByteString)
  }

-- * Cancellation

-- | A cancellation handle, isolated from the connection that produced it,
-- hence usable from another thread while that connection is busy.
data Cancel = Cancel
  { -- | Request cancellation of the in-progress command via the handle.
    cancel :: IO (Either ByteString ())
  }

-- * Connection

-- | One flat capability record: closing, inspecting, querying, escaping,
-- async commands, pipelining, cancellation handles, notifications, copy,
-- large objects, control.
--
-- Produced by the 'connectdb'\/'connectStart'\/'newNullConnection' fields of
-- an t'Adapter', which close each field over their own connection
-- representation (a C @PGconn@ pointer, a native socket, ...).
data Connection = Connection
  { -- | Drive an asynchronous connection attempt forward.
    connectPoll :: IO PollingStatus,
    -- | Whether the connection is the null sentinel.
    isNullConnection :: Bool,
    -- | Close the connection and release its resources.
    finish :: IO (),
    -- | Reset the communication channel to the server (blocking).
    reset :: IO (),
    -- | Begin resetting the connection asynchronously.
    resetStart :: IO Bool,
    -- | Drive an asynchronous reset forward.
    resetPoll :: IO PollingStatus,
    -- | The database name of the connection.
    db :: IO (Maybe ByteString),
    -- | The user name of the connection.
    user :: IO (Maybe ByteString),
    -- | The password of the connection.
    pass :: IO (Maybe ByteString),
    -- | The server host name of the connection.
    host :: IO (Maybe ByteString),
    -- | The port of the connection.
    port :: IO (Maybe ByteString),
    -- | The command-line options passed in the connection request.
    options :: IO (Maybe ByteString),
    -- | Current connection status.
    status :: IO ConnStatus,
    -- | Current in-transaction status of the server.
    transactionStatus :: IO TransactionStatus,
    -- | Look up a current parameter setting reported by the server.
    parameterStatus :: ByteString -> IO (Maybe ByteString),
    -- | The frontend\/backend protocol version.
    protocolVersion :: IO Int,
    -- | The server version, as an integer of the form @MMmmpp@.
    serverVersion :: IO Int,
    -- | The most recent error message, if any. Formatted locally by the driver,
    -- so adapters are not expected to agree byte for byte.
    errorMessage :: IO (Maybe ByteString),
    -- | The file descriptor of the connection socket.
    socket :: IO (Maybe Fd),
    -- | The process ID of the backend serving this connection.
    backendPID :: IO Int32,
    -- | Whether the connection authentication method required a password but
    -- none was available.
    connectionNeedsPassword :: IO Bool,
    -- | Whether the connection authentication used a password.
    connectionUsedPassword :: IO Bool,
    -- | Submit a command and wait for the result.
    exec :: ByteString -> IO (Maybe Result),
    -- | Submit a parameterized command. Each parameter is given as
    -- @(type oid, value, format)@, or 'Nothing' for SQL @NULL@. The final
    -- 'Format' selects the result format.
    execParams ::
      ByteString ->
      [Maybe (Word32, ByteString, Format)] ->
      Format ->
      IO (Maybe Result),
    -- | Prepare a named statement. The OID list, when supplied, fixes parameter
    -- types; 'Nothing' leaves them to be inferred.
    prepare :: ByteString -> ByteString -> Maybe [Word32] -> IO (Maybe Result),
    -- | Execute a previously prepared statement. Each parameter is
    -- @(value, format)@, or 'Nothing' for SQL @NULL@.
    execPrepared ::
      ByteString ->
      [Maybe (ByteString, Format)] ->
      Format ->
      IO (Maybe Result),
    -- | Describe a prepared statement.
    describePrepared :: ByteString -> IO (Maybe Result),
    -- | Describe a portal.
    describePortal :: ByteString -> IO (Maybe Result),
    -- | Escape a string for safe inclusion in an SQL literal.
    escapeStringConn :: ByteString -> IO (Maybe ByteString),
    -- | Escape binary data for use within a @bytea@ literal.
    escapeByteaConn :: ByteString -> IO (Maybe ByteString),
    -- | Escape a string for use as an SQL identifier (e.g. a table or column
    -- name), including the surrounding double quotes.
    escapeIdentifier :: ByteString -> IO (Maybe ByteString),
    -- | Submit a command without waiting for the result.
    sendQuery :: ByteString -> IO Bool,
    -- | Asynchronous 'execParams'.
    sendQueryParams :: ByteString -> [Maybe (Word32, ByteString, Format)] -> Format -> IO Bool,
    -- | Asynchronous 'prepare'.
    sendPrepare :: ByteString -> ByteString -> Maybe [Word32] -> IO Bool,
    -- | Asynchronous 'execPrepared'.
    sendQueryPrepared :: ByteString -> [Maybe (ByteString, Format)] -> Format -> IO Bool,
    -- | Asynchronous 'describePrepared'.
    sendDescribePrepared :: ByteString -> IO Bool,
    -- | Asynchronous 'describePortal'.
    sendDescribePortal :: ByteString -> IO Bool,
    -- | Collect the next result from an asynchronous command.
    getResult :: IO (Maybe Result),
    -- | Read input from the server into the driver's buffer.
    consumeInput :: IO Bool,
    -- | Whether a command is busy (a 'getResult' would block).
    isBusy :: IO Bool,
    -- | Set the non-blocking flag of the connection.
    setnonblocking :: Bool -> IO Bool,
    -- | Whether the connection is in non-blocking mode.
    isnonblocking :: IO Bool,
    -- | Select single-row mode for the currently executing query.
    setSingleRowMode :: IO Bool,
    -- | Flush queued output data to the server.
    flush :: IO FlushStatus,
    -- | Current pipeline-mode status.
    pipelineStatus :: IO PipelineStatus,
    -- | Enter pipeline mode.
    enterPipelineMode :: IO Bool,
    -- | Leave pipeline mode.
    exitPipelineMode :: IO Bool,
    -- | Mark a synchronization point in a pipeline.
    pipelineSync :: IO Bool,
    -- | Request the server to flush its output buffer in pipeline mode.
    sendFlushRequest :: IO Bool,
    -- | Obtain a cancellation handle for the connection.
    getCancel :: IO (Maybe Cancel),
    -- | Return the next notification from the queue, if any.
    notifies :: IO (Maybe Notify),
    -- | Stop accumulating notices for retrieval via 'getNotice'.
    disableNoticeReporting :: IO (),
    -- | Start accumulating notices for retrieval via 'getNotice'.
    enableNoticeReporting :: IO (),
    -- | Retrieve the next accumulated notice, if any.
    getNotice :: IO (Maybe ByteString),
    -- | Send data on a @COPY FROM STDIN@ connection.
    putCopyData :: ByteString -> IO CopyInResult,
    -- | Signal the end of @COPY FROM STDIN@; 'Just' aborts with the given error.
    putCopyEnd :: Maybe ByteString -> IO CopyInResult,
    -- | Receive data on a @COPY TO STDOUT@ connection. The 'Bool' selects
    -- non-blocking mode.
    getCopyData :: Bool -> IO CopyOutResult,
    -- | Create a new large object.
    loCreat :: IO (Maybe Word32),
    -- | Create a new large object with the given OID.
    loCreate :: Word32 -> IO (Maybe Word32),
    -- | Import a file as a new large object.
    loImport :: FilePath -> IO (Maybe Word32),
    -- | Import a file as a new large object with the given OID.
    loImportWithOid :: FilePath -> Word32 -> IO (Maybe Word32),
    -- | Export a large object to a file.
    loExport :: Word32 -> FilePath -> IO (Maybe ()),
    -- | Open a large object.
    loOpen :: Word32 -> IOMode -> IO (Maybe Int32),
    -- | Write to an open large object.
    loWrite :: Int32 -> ByteString -> IO (Maybe Int),
    -- | Read from an open large object.
    loRead :: Int32 -> Int -> IO (Maybe ByteString),
    -- | Seek within an open large object.
    loSeek :: Int32 -> SeekMode -> Int -> IO (Maybe Int),
    -- | Report the current seek position of an open large object.
    loTell :: Int32 -> IO (Maybe Int),
    -- | Truncate an open large object.
    loTruncate :: Int32 -> Int -> IO (Maybe ()),
    -- | Close an open large object.
    loClose :: Int32 -> IO (Maybe ()),
    -- | Remove a large object.
    loUnlink :: Word32 -> IO (Maybe ()),
    -- | The current client encoding name.
    clientEncoding :: IO ByteString,
    -- | Set the client encoding.
    setClientEncoding :: ByteString -> IO Bool,
    -- | Set error verbosity, returning the previous setting.
    setErrorVerbosity :: Verbosity -> IO Verbosity
  }

-- * Adapter

-- | An adapter package's connection-establishing functions, bundled into one
-- value. Each adapter package (e.g. @pqi-ffi@, @pqi-native@) exports exactly
-- one value of this type, conventionally named @adapter@.
--
-- This is the only value that identifies an adapter: t'Connection', t'Result'
-- and t'Cancel' say nothing about which adapter produced them. Callers that
-- must stay adapter-agnostic - a differential test harness, a library letting
-- its users pick a driver at runtime - pass an t'Adapter' around instead of a
-- family of adapter-qualified top-level functions.
data Adapter = Adapter
  { -- | A short identifier for the adapter (e.g. @\"pqi-ffi\"@), for use in
    -- test descriptions, logs, and error messages.
    name :: Text,
    -- | Connect to a database, as @PQconnectdb@.
    connectdb :: ByteString -> IO Connection,
    -- | Begin connecting to a database asynchronously, as @PQconnectStart@.
    connectStart :: ByteString -> IO Connection,
    -- | The null-sentinel connection, as @PQnewNullConnection@.
    newNullConnection :: IO Connection,
    -- | Convert the textual representation of a @bytea@ value, as produced
    -- by the server, back into raw bytes, as @PQunescapeBytea@.
    unescapeBytea :: ByteString -> IO (Maybe ByteString),
    -- | Render an 'ExecStatus' as the string describing its status code
    -- (e.g. @\"PGRES_TUPLES_OK\"@), as @PQresStatus@.
    resStatus :: ExecStatus -> IO ByteString
  }

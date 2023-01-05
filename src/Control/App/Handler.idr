module Control.App.Handler

import Control.App

export
handleErr : App (err :: e) a -> (onerr : err -> App e a) -> App e a
handleErr app onerr = handle app pure onerr

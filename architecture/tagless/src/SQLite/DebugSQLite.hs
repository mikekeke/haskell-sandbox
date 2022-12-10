module SQLite.DebugSQLite where
  
import Database.SQLite.Simple (open, execute_, execute)
import Types (mkUser, )
import App.SQLiteInstances ()

run :: IO ()
run = do
  conn <- open "app.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS person (name TEXT, phone TEXT)"
  execute conn "INSERT INTO person (name, phone) VALUES (?,?)" (mkUser "phone-1" "Bob" "3311")
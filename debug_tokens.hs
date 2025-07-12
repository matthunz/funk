import Funk.Token

main :: IO ()
main = do
  let expr = "let x = \\y . y;"
  case tokenize expr of
    Left err -> putStrLn $ "Error: " ++ show err
    Right tokens -> do
      putStrLn $ "Tokens (" ++ show (length tokens) ++ "):"
      mapM_ print tokens

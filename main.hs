{-
                           Práctica Haskell
--------------------------------------------------------------------------------
AUTOR:  Dionisio Martínez Alberquilla

REQUIERE:

        - HuffmanCod.hs
        - RsaCrypto.hs

IMPLEMENTACIÓN:

        - algoritmo RSA
        - codificación Huffman

USO:

        El programa permite cifrar/descifrar y codificar/descodificar cualquier
        fichero de texto mediante un menú donde se pueden escoger diferentes
        opciones. Todos los datos generados en el proceso (árbol de Huffman,
        claves pública y privada...) se almacenan en ficheros de texto.
        La opción (3) disponible en el menú comprime un fichero de texto
        (mensaje.txt), lo encripta y realiza las operaciones inversas, dando
        como salida el fichero original_file.txt que deberá coincidir con el
        mensaje original.

        Los ficheros p.txt y q.txt contienen dos números primos de 1024 bits
        para generar las claves en el cifrado RSA.

NOTA:

        Algunas operaciones de lectura/escritura requieren cierto tiempo.

--------------------------------------------------------------------------------
-}

import HuffmanCod
import RsaCrypto

readInt :: IO Int
readInt = do n <- getLine
             return (read n)

menu :: IO ()
menu = do putStrLn "                      Select option:\n "
          putStrLn "             Encode/decode              -> 1"
          putStrLn "             Encrypt/decrypt            -> 2"
          putStrLn "             Automatic test (all cases) -> 3"
          putStrLn "             Exit                       -> 4"
          option <- readInt
          case option of
            1 -> do putStrLn "Encode -> 1"
                    putStrLn "Decode -> 2"
                    opt <- readInt
                    case opt of
                      1 -> do putStrLn "Filename to encode:"
                              fileIn <- getLine
                              putStrLn "Save as:"
                              fileOut <- getLine
                              mainHuff True fileIn fileOut
                              putStr "\n\n"
                              menu
                      2 -> do putStrLn "File to decode:"
                              fileIn <- getLine
                              putStrLn "Save as:"
                              fileOut <- getLine
                              mainHuff False fileIn fileOut
                              putStr "\n\n"
                              menu
            2 -> do putStrLn "Encrypt  -> 1"
                    putStrLn "Decrypt  -> 2"
                    opt <- readInt
                    case opt of
                      1 -> do putStrLn "Filename to encrypt:"
                              fileIn <- getLine
                              putStrLn "Save as:"
                              fileOut <- getLine
                              mainRsa True fileIn fileOut
                              putStr "\n\n"
                              menu
                      2 -> do putStrLn "File to decrypt:"
                              fileIn <- getLine
                              putStrLn "Save as:"
                              fileOut <- getLine
                              mainRsa False fileIn fileOut
                              putStr "\n\n"
                              menu
            3 -> do mainTest
                    putStr "\n\n"
                    menu
            4 -> return ()


mainHuff :: Bool -> String -> String -> IO ()
-- Encode
mainHuff True fileIn fileOut  = do text <- readFile fileIn
                                   putStr "Encoding.............."
                                   let (compress, (huffmanTree, codesMap)) =
                                        encodeHuff text
                                   writeFile "huffman_tree.txt"
                                             (show huffmanTree)
                                   writeFile "huffman_map.txt" (show codesMap)
                                   writeFile fileOut (show compress)
                                   putStrLn "    [OK]"
-- Decode                        
mainHuff False fileIn fileOut = do mapFromText <- readFile "huffman_map.txt"
                                   huffmanTree <- readFile "huffman_tree.txt"
                                   compressText <- readFile fileIn  
                                   putStr "Decoding.............."         
                                   let uncompress = decodeHuff
                                                        (read compressText,
                                                        (read huffmanTree,
                                                         read mapFromText))
                                   putStrLn "    [OK]"                       
                                   writeFile fileOut (uncompress)

mainRsa :: Bool -> String -> String -> IO ()
-- Encrypt
mainRsa True fileIn fileOut  = do p <- readFile "p.txt"
                                  q <- readFile "q.txt"
                                  let keys = genKeys (read p) (read q)                      
                                  msg <- readFile fileIn
                                  writeFile "Public-key.txt" (show (fst keys))
                                  writeFile "Private-key.txt" (show (snd keys))
                                  putStr "Encrypting............"          
                                  let msg_c = encrypt (fst keys)
                                                      (stringToBlocks msg)
                                  putStrLn "    [OK]"
                                  writeFile fileOut (show msg_c)
-- Decrypt                      
mainRsa False fileIn fileOut = do p_key <- readFile "Private-key.txt"
                                  cipherText <- readFile fileIn
                                  putStr "Decrypting............"
                                  let msg_des =
                                        blocksToString
                                            (decrypt (read p_key)
                                                     (read cipherText))
                                  putStrLn "    [OK]"      
                                  writeFile fileOut (msg_des)

mainTest :: IO ()
mainTest = do mainHuff True  "mensaje.txt" "compressed_file.txt"
              mainRsa  True  "compressed_file.txt" "encrypted_file.txt"
              mainRsa  False "encrypted_file.txt" "decrypted_file.txt"
              mainHuff False "decrypted_file.txt" "original_file.txt"

main :: IO ()
main = do menu

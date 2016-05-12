import System (getArgs)
import Prelude
import IO
import Char
import Converte


separarVermelho :: Int -> Int -> Float
separarVermelho x = x - 0x78 

separarVerde :: Int -> Int -> Float
separarVerde x = x - 0x77 

separarAzul :: Int -> Int -> Float
separarAzul x = x - 0x76 


conv :: [Int] -> [Float]
conv [] = []
conv x = do
		let listas = splitAt 2 x

main :: IO ()
main = do
	args <- getArgs
	case args of
		[arquivo] -> do
			entrada <- readFile arquivo
			saida <- openFile "imagem.tiff" WriteMode

			let strDados = snd (splitAt 44 (map ord entrada))

			let vermelho = separarVermelho(dados)
                        let verde = separarVerde(dados)
                        let azul = separarAzul(dados)

			hPrint saida (dados)
			hClose saida
		otherwise -> putStrLn "Uso: separados ARQUIVO"

data ClienteQChega = Nao | Sim TempoQchegou TempoPAtend
type TempoPAtend = Int
type TempoQchegou = Int

data ClienteQSai = Nenhum |Liberado TempoQchegou TempoDEsp TempoPAtend
type TempoDEsp = Int


type Fila = [Int]

estaVazia :: Fila -> Bool
estaVazia [] = True
estaVazia _ = False


--Qual das duas implementações à seguir de enfileirar e desenfileirar é mais eficiente?
-- Implementação 1:
enfileira :: Int -> Fila -> Fila
enfileira a x = x ++ [a]
desenfileira :: Fila -> (Int,Fila)
desenfileira x
 | not (estaVazia x) = (head x,tail x)
 | otherwise = error "erro: a fila esta vazia"




-- Implementação 2:
enfileira' :: Int-> Fila -> Fila
enfileira' a x = (a:x)
desenfileira' :: Fila->(Int,Fila)
desenfileira' x
 | not (estaVazia x) = (last x,init x)
 | otherwise = error "erro: a lista esta vazia"


--A primeira implementação é mais eficiente em desenfileirar e e menos em enfileirar, já a segunda
--é mais eficiente em em enfileirar  menos em desenfileirar.
--  Isso ocorre porque na primeira implementação de enfileirar na verdade concatena duas listas
-- e a velocidade da operação "++" depende do tamanho da Fila, porque preisa percorrer a fila
-- inteira para inserir o elemento (transformado em lista) no final da fila.
--Veja como o operado "++" é implementado:

maismais []  xs = xs
maismais (e:es) xs = e : (maismais es xs)

-- Já o operador ":" recebe um elemento e uma fila, e inserere esse elemento diretamente no final da fila sem a 
-- necessidade de percorrer a fila inteiramente.


--A primeira implementação é mais eficiente em desenfileirar pois oos operadores "head" e "tail" trabalham apenas com
--remoção de elementos. "head" retorna apenas o primeiro elementod a lista e "tail" apaga o primeira elemento e retorna o resto da lista
--sem o primeiro elemento.
--



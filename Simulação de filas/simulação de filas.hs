

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
 |not (estaVazia x) = (head x,tail x)
 |otherwise = error "erro: a fila esta vazia"

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

-- Já o operador ":" recebe um elemento e uma fila, e inserere esse elemento diretamente no início da fila sem a 
-- necessidade de percorrer a fila inteiramente. Por isso, a segunda implementação é mais eficiente em enfileirar.

--A primeira implementação é mais eficiente em desenfileirar pois os operadores "head" e "tail" trabalham apenas com
--com elementos e não listas. "head" retorna apenas o primeiro elementod a lista e "tail" apaga o primeira elemento e retorna o resto da lista
--sem o primeiro elemento.
--A segunda implementação de desenfileirar é menos eficiente porque o operador "last" precisa percorerrer
--a fila inteira para retornar o último elemento. Assim sendo, demora mais tempo para ser executada quanto maior a fila.


estaVazia1 ([],[]) = True
estaVazia1 _ = False

enfileira1 a (l,r) = (l,(a:r))

desenfileira1 ((a:l),r) = (a,(l,r))
desenfileira1 ([],[]) = error "erro: fila vazia"
desenfileira1 ([],r) = desenfileira1 (reverse r,[])

--só uma obersavação rápida aqui, nesse algoritmo de enfileirar, o parametro é inserido na segunda fila
-- no início. Ou seja, o primeiro da fila é o último da lista.
-- Na hora de desenfileirar, a lista é invertida e em seguida é removido o primeiro dessa lista invertida
-- que seria o próximo da fila na realidade.

data ClienteQChega = Nao | Sim TempoQchegou TempoPAtend deriving(Eq,Show,Ord)
type TempoPAtend = Int
type TempoQchegou = Int

data ClienteQSai = Nenhum |Liberado TempoQchegou TempoDEsp TempoPAtend deriving(Eq,Show,Ord)
type TempoDEsp = Int

--Operações para A fila

type EstadoDaFila = (Tempo,TempoDeAtend,[ClienteQChega])
type Tempo = Int -- tempo atual
type TempoDeAtend = Int --tempo de atendimento até o momento 

adicionaCliente :: ClienteQChega -> EstadoDaFila -> EstadoDaFila
adicionaCliente m (tempo, tempoDeAtend, m1) = (tempo,tempoDeAtend, m1 ++ [m])

processaFila ::EstadoDaFila -> (EstadoDaFila,[ClienteQSai])
processaFila (tempo,tempDeAtend,[]) = ((tempo+1,tempDeAtend,[]),[])
processaFila (tempo,tempDeAtend, ((Sim a tempNecDAtend):resto))
 | tempDeAtend < tempNecDAtend = ((tempo+1,tempDeAtend+1, ((Sim a tempNecDAtend):resto)),[])
 | otherwise = ((tempo+1,0,resto),[Liberado a (tempo-tempNecDAtend-a) tempNecDAtend])


filaDeInicio :: EstadoDaFila
filaDeInicio = (0,0,[])

tamanhoDaFila :: EstadoDaFila -> Int
tamanhoDaFila (tempo,tempoDeAtend,l) = length l

filaVazia :: EstadoDaFila -> Bool
filaVazia (t,s,q) = (q == [])


--operações para o servidor
type EstadoDoServidor = [EstadoDaFila]
copy n x = take n (repeat x)

estadoInicialDoServidor :: EstadoDoServidor
estadoInicialDoServidor = copy nroDeFilas filaDeInicio

nroDeFilas :: Int
nroDeFilas = 4 -- é isso que tem que mudar na primeira questão da simulação

tamanhoDoServidor :: EstadoDoServidor -> Int
tamanhoDoServidor = length

colocaNaFila :: Int -> ClienteQChega -> EstadoDoServidor -> EstadoDoServidor
colocaNaFila n im st = take n st ++ [novoEstadoDaFila] ++ drop (n+1) st
  where novoEstadoDaFila = adicionaCliente im (st!!n)

processaServidor :: EstadoDoServidor -> (EstadoDoServidor, [ClienteQSai])
processaServidor [] = ([],[])
processaServidor (q:qs) = ((nq:nqs), mess ++ messes)
 where (nq,mess) = processaFila q
       (nqs,messes) = processaServidor qs


processaSimulacao :: EstadoDoServidor -> ClienteQChega -> (EstadoDoServidor,[ClienteQSai])
processaSimulacao estServ im = (adicionaNovoObjeto im estServ1,clientQSai)
 where (estServ1,clientQSai) = processaServidor estServ

menorFila :: EstadoDoServidor -> Int
menorFila [q] = 0
menorFila (q:qs)
 | tamanhoDaFila (qs!!menor) <= tamanhoDaFila q = menor + 1
 | otherwise = 0
 where menor = menorFila qs


adicionaNovoObjeto :: ClienteQChega -> EstadoDoServidor-> EstadoDoServidor
adicionaNovoObjeto Nao estServ = estServ
adicionaNovoObjeto (Sim tempoDeChegada tempoNecAtend) estServ = colocaNaFila (menorFila estServ) (Sim tempoDeChegada tempoNecAtend) estServ


--geração de valores aleatórios:
semente :: Integer 
semente = 17489
multiplicador :: Integer
multiplicador = 25173
incremento :: Integer
incremento = 13849
modulo :: Integer
modulo = 65536

proxNumAleat :: Integer -> Integer
proxNumAleat n = (multiplicador*n + incremento) `rem` modulo

seqAleatoria :: (Integer -> [Integer])
seqAleatoria semente = iterate (proxNumAleat) semente

dist :: Num t => [(t, Float)]
                         
dist = [(1,0.2),(2,0.25),(3,0.25),(4,0.15),(5,0.1),(6,0.05)]
                     
escalaSequencia :: Integer-> Integer-> ([Integer] -> [Integer])
escalaSequencia a b = map  scala 
 where
     scala n = (div n denom) + a
     faixa = b - a + 1 
     denom = div modulo faixa

geraFuncao :: [(t,Float)] -> (Float -> t)
geraFuncao dist = geraFun dist 0.0
geraFun ((ob,p):dist) nUlt aleat
 | nProx >= aleat && aleat > nUlt = ob
 |otherwise = geraFun dist nProx aleat
   where nProx = (p* (fromInteger modulo) + nUlt)
                     
--SIMULAÇÃO:
simule :: EstadoDoServidor -> ([ClienteQChega] -> [ClienteQSai])
simule estDoServ (im:messes) = outmesses ++ simule proxEstDoServ messes
 where (proxEstDoServ,outmesses) = processaSimulacao estDoServ im

seqDeTempos :: [TempoPAtend]                                    
seqDeTempos = map (geraFuncao dist . fromInteger) (seqAleatoria semente)

entradaDaSimulacao = zipWith Sim [ 1..] seqDeTempos

entradaDaSimulacao2 = take 50 entradaDaSimulacao ++ naos

naos = (Nao:naos)

tempoDeEsperaTotal :: ([ClienteQSai] -> Int)
tempoDeEsperaTotal = sum . map tempoDEsp
 where
    tempoDEsp (Liberado _ w _) = w

--1)R:
-- considerando nroDefilas = 1
--   tempoDeEsperaTotal (take 50 (simule estadoInicialDoServidor entradaDaSimulacao2))
--    com 1 fila = 3358 minutos

--2)R: O tempo total de espera será zero à partir de 5 filas nesse caso. 
      
--3)R: Para essa questão eu vou precisar alterar a função adicionaNovoObjeto
--a função simule e a função processa simulação da seguinte maneira:

adicionaNovoObjetoRound :: ClienteQChega -> EstadoDoServidor-> EstadoDoServidor
adicionaNovoObjetoRound Nao estServ = estServ
adicionaNovoObjetoRound (Sim tempoDeChegada tempoNecAtend) estServ = colocaNaFila (fazRound tempoDeChegada)

 (Sim tempoDeChegada tempoNecAtend) estServ
fazRound n = n `mod` nroDeFilas

processaSimulacaoRound :: EstadoDoServidor -> ClienteQChega -> (EstadoDoServidor,[ClienteQSai])
processaSimulacaoRound estServ im = (adicionaNovoObjetoRound im estServ1,clientQSai)
 where (estServ1,clientQSai) = processaServidor estServ

simuleRound :: EstadoDoServidor -> ([ClienteQChega] -> [ClienteQSai])
simuleRound estDoServ (im:messes) = outmesses ++ simuleRound proxEstDoServ messes
 where (proxEstDoServ,outmesses) = processaSimulacaoRound estDoServ im


-- tempoDeEsperaTotal (take 50 (simuleRound estadoInicialDoServidor entradaDaSimulacao2))
-- considerando total de filas =1 , tempo total é de 3358 minutos.
-- O tempo de espera será zero à partir de 7 filas.


--4) Redefinindo EstadoDoServidor com um alimentador de filas.
type EstadoDoServidor2 = ([EstadoDaFila], EstadoDaFila)

tamanhoDoServidor2 :: EstadoDoServidor2 -> Int
tamanhoDoServidor2 = length . fst





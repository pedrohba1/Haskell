
Questão 1:
func =
	let
	 tuplas = [(1,8),(2,5),(0,1),(4,4),(3,2)]
	 h = (foldr1(+)).map(\(x,y)->x*y-1).filter(\(x,_)-> even x)
	in h tuplas

	-- 4*4-1  + 0*1 -1 + 2*5 -1
	-- 9 -1 + 15
	-- 23

1.A)	4 funções de alta Ordem :
	.foldr1
	.map
	.filter
	. "."

1.B) [(2,5),(0,1),(4,4)] => 4*4 -1 + 0*1 -1 + 2*5 -1 == 23


QUESTÃO 2 -- Eu nao tenho certeza se essa resposta esta certa, alguem confere também por favor!


polimorfismo Parametrico: É aplicado sobre Tipos Algebricos de Dados (qualquer tipo). A função polimórfica paramétrica não está
vinculada ao tipo de dado, e sim à estrutura que está sendo passada. 
Exemplos:
 head [a] -> a 
tail [a] -> [a]
polimorfismo ad-hoc: É aplicado sobre tipos restritos. 
Exemplos:
(+) :: Num a => a ->a ->a 
sum :: Num a => [a] -> a
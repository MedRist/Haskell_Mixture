data BT a= E|N a (BT a )(BT a)


{--Prefix Parcours--}
parPref :: BT a -> [a]
parPref E=[]
parPref (N x left right)= [x]++(parPref left)++(parPref right)


{--Infix Parcours--}
infP :: BT a -> [a]
infP E=[]
infP (N x left right)= (infP left)++[x]++(infP right)


{--Post Parcours--}
post :: BT a -> [a]
post E=[]
post (N x left right)= (post left)++(post right)++[x]




{--Build BTA form a list --}

build :: (Ord a)=> [a]->BT a

build []=E
build [x]=N x E E
build (x:xs)=add x(build xs)


bd xs =foldr (add)E xs



{--Add an element to BT a --}

add :: (Ord a)=> a-> BT a ->BT a
add x E=N x E E
add x (N a left right)| x==a =N x left right
                      | x<a  =N a (add x left)right
                      |otherwise =N a left (add x right)



{--min of BTS--}

-- Minimum d'un arbre
minT (N x E _) = x
minT (N _ l _) = minT l

{--delete element from BT--}
delete x (N y E r) | x == y = r
delete x (N y l E) | x == y = l
delete x (N y l r) | x < y  = N y (delete x l) r
                      | x > y  = N y l (delete x r)
                      | x == y = N k l (delete k r)
                      where
                        k = minT r




{--Root of a tree --}
root :: BT a -> a
root E=error "Empty tree"
root (N x _ _)=x



murge :: (Ord a)=>BT a->BT a->BT a
murge E E =E
murge E (N a left right )=N a left right
murge  (N a left right ) E=N a left right
murge (N x l r) (N y j k)=build (infP (N x l r)++infP (N y j k) )

zip' :: [a]->[b]->[(a,b)]
zip' [] xs=[]
zip' xs []=[]
zip' (x:xs)(y:ys)=(x,y):zip' xs ys



takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p []=[]
takeWhile' p(x:xs)| p x  =x:takeWhile' p xs
                  |otherwise = []



posi ::Eq a=> a->[a]->[Int]
posi x []=[]
posi x xs=[i|(x',i)<- zip' xs [0..n], x==x']
                      where n= length xs



{--Insert in a ordred list--}
insert :: Ord a=> a->[a]->[a]
insert x[]=[x]
insert x(y:ys) |x<=y = x:(y:ys)
               |otherwise = y:insert x ys

{--Searching for an element--}
find ::Eq a=> [a]->a->Bool
find []_=False
find (x:xs)y |x==y =True
             |otherwise = find xs y


{--Insertion Sort--}
iSort :: Ord a => [a]->[a]
iSort []=[]
iSort (x:xs)= insert x(iSort xs)


{--Quick Sort--}
qSort :: Ord a => [a]->[a]
qSort []=[]
qSort (x:xs)=qSort inf ++[x]++ qSort sup
            where inf=[y | y<- xs , y<=x]
                  sup=[y | y<- xs, y>x]
{--By using Filters--}
quickSort (x:xs)= quickSort(filter(\(_,a)->a<= snd(x) xs)++[x]++ quickSort(filter(\(_,a)->a > snd(x) xs)


{--delete an element from list --}
delete :: Eq a=> a->[a]->[a]
delete x []=[]
delete y(x:xs)| y==x = delete y xs
              | otherwise =x: delete y xs


minim :: Ord a => [a]->a
minim []=error "liste vide"
minim [x]=x
minim (x:xs)= min x (minim xs)



{--Selection Sort--}
sSort :: Ord a => [a]->[a]
sSort []=[]
sSort xs = y:sSort xs'
          where y=minim xs
                xs'= delete y xs

let rec flatten1(list) =
  if list=[] then []
  else List.hd list @ flatten1 (List.tl list)
;;

flatten1([[5;6];[1;2;3]])=[5; 6; 1; 2; 3];;
flatten1([[];[]])=[];;
flatten1([['a';'l'];['a']])=['a';'l';'a'];;

let rec count(x,list) =
  if list=[] then 0
  else (if x=List.hd list then 1 else 0) 
  + count(x, List.tl list)
;;
count('a',['a';'l';'a'])=2;;
count('z',[])=0;;
count(4,[3;4;2;4;6;4])=3;;

let rec replicate(x, n) = 
  if n<0 then failwith "Negative numer of repetitions"
  else if n = 0 then []
  else x :: replicate(x,n-1)
;;

replicate('a',3)=['a';'a';'a'];;
replicate(2, 4)=[2;2;2;2];;
replicate(0,0)=[]

let rec sqrList(list) =
  if list = [] then []
  else List.hd list * List.hd list :: sqrList(List.tl list)
;;

sqrList([1;2;3])=[1;4;9];;  
sqrList([0;2;4])=[0;4;16];;
sqrList([])==[];;

let palindrome(list) =
  if list = [] then false
  else if List.rev list = list then true
  else false
;;

palindrome([1;2;2;1])=true;;
palindrome([])=false;;
palindrome(['l';'a';'s'])=false;;

let rec listLength(list) = 
  if list=[] then 0
  else 1 + listLength(List.tl list)
;;

listLength(['p';'w';'r'])=3;;
listLength([])=0;;
listLength([1;2;3;4;5;6])=6;;
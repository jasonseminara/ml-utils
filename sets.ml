(*Author Jason Seminara 2014-04-08*)
(*signature SET = sig
    exception E
    type set
    type el
    val empty       :   set
    val insert      :   set -> el -> set
    val remove      :   set -> el -> set
    val member      :   set -> el -> bool
    val fold        :   (el -> el -> el) -> el -> set -> el
    val union       :   set -> set -> set
    val intersect   :   set -> set -> set
    val setexists   :   set -> (set -> bool) -> bool
    val setall      :   set -> (set -> bool) -> bool
    val subset      :   set -> set set
    val singleton	:	el -> set
end;*)

(* --------------------------------------------------- *)

(*functor ListSet(type et) :>SET = struct*)

structure ListSet = 
struct

	exception E;

    type 'a set = 'a list;
       
       fun singleton a = [a];
        (* empty *)
        (* evaluates to an empty Set.*)
        val empty = [];

        

        
        (* subset *)
        (* splits any set into a 'power set' (i.e. a set containing every possible subset) *)
        (* This is a direct encoding of 'The subsets of S are all the subsets of A, plus all the subsets of A 
           with x added as an element.'  *)
        fun subset [] = [[]]
          | subset (x::xs) = 
            let 
                val tailsubsets = subset xs
                fun add a b = a :: b;
            in 
                tailsubsets @ map (add x) tailsubsets
            end

        

        (* fold *)
        (* similar to foldl and foldr discussed in the lecture, only operates
        on a set instead of a list. The arguments accepted by fold should be
        a function, a seed value, and a set. The result should be a single value
        whose type is the same as the members of the set. There is no need for
        the user to specify whether to fold left or right due to the fact that sets
        are unordered. (You may fold the elements in any arbitrary order on the
        underlying set type.) *)
        fun fold F acc []      = acc
          | fold F acc (x::xs) = fold F (F acc x) xs;
            

        (* find aux func *)
        fun findAll a comp i = (a=i) andalso comp;
        fun findOne a comp i = (a=i) orelse comp;

        (* helper predicates *)
        (* isEven? *)
        fun isEven i = i mod 2 = 0;

        (* allEven? *)  
        fun allEven [] = false
          | allEven st = List.all isEven st;


        val addInt = (op+);
        fun zeroSum [] = false
          | zeroSum st = (foldr addInt 0 st) = 0;
            

        (* member? *)
        (* accepts a set and an item for consideration. Evaluates to a
        Boolean representing whether the set contains the item.*)
        fun member st a    = List.exists (fn i => i = a) st;

        fun notmember st a = not (member st a);

        (* union *)
        (* accepts two sets as arguments. Evaluates to a new set containing
        the union of the members of the two input sets (i.e., a set consisting of
        members from either input set).*)
        fun union nil listB = listB
          | union listA nil = listA
          (* make a list of items in a that are not members of listB *)
          (* append this list to listB *)
          | union listA listB  = List.filter (notmember listB) listA @ listB;


        (* intersect *)
        (* accepts two sets as arguments. Evaluates to a new set containing
        the union of the members of the two input sets (i.e., a set consisting of
        members from either input set).*)
        fun intersect [] listB = listB
          | intersect listA [] = listA
          (* make a list of items in a that are already members of b *)
          | intersect listA listB  = List.filter (member listB) listA;


        (* remove *)
        (* accepts a set and an item for removal as arguments. Evaluates
            to a new set with the item removed, provided the item existed. Evaluates
            to the original set if the item doesn’t exist in the set.*)
        fun remove st a = List.filter (fn i=> a<>i) st;

        (* insert *)
        (* accepts a set and item for insertion as arguments. Evaluates to
            a new set with the new item inserted, provided the item does not already
            exist in the set. If the item is already in the set, then it evaluates to the
            original set.*)
        (* we'll simply use the Union function to insert the item after we promote it to a list*)
        fun insert st a = union st (singleton a);

	
        (* setExists *)
        (* accepts a set and a predicate (which itself accepts a set as
        an argument and evaluates to a Boolean). Evaluates to a Boolean that
        expresses whether any subset of the original set satisﬁes the predicate.*)
        fun setexists st pred = List.exists pred (subset st);

        (* setAll *)
        (* accepts a set and a predicate (which itself accepts a set as
        an argument and evaluates to a Boolean). Evaluates to a Boolean that
        expresses whether all subsets of the original set satisfy the predicate.*)
        fun setall st pred = List.all pred (subset st);


        fun subsetSum st = setexists st zeroSum;

        fun find_subsetSum st = 
            let 
                val foundItem = List.find zeroSum (subset st)
            in
                if isSome foundItem 
                then {found=true , witness= valOf foundItem}
                else {found=false, witness= empty}
            end;

    end;

print("================TESTS=================\n");

(*structure intSet = ListSet(type et = int);
structure charSet = ListSet(type et = char);
structure colorSet = ListSet(type et = color);


val setOfChars = charSet.empty;


map charSet.insert [#"A", #"B", #"C"];
charSet.subset setOfChars;


val isMember = true = member charSet #"C";
val isMember = false = member charSet #"d";*)

open ListSet;

val intSet = empty;
val intSet = insert (insert (insert (insert intSet 2) 3) 4)5;
(*should be true*)
val mbr3 = true = member intSet 3;
(*should be false*)
val mbr6 = false = member intSet 6;

subset [2,3,5];

val even = true = isEven 4;
val odd = false = isEven 99;

val allEven33 = false = allEven [3,3];
val allEven23 = false = allEven [2,3];
val allEvennull = false = allEven [];

val allf = false = allEven [2,4,5,6,9,234];
val allE = true  = allEven [2,4,6,0,234];

insert [#"A",#"B",#"C",#"D"] #"E";
insert [2,3,4,5] 5;
insert [2,3,4,5] 6;
union [1,2,3,4,5] [6,7,8,9,10];
union [2,4,6,8] [6,7,8,9,10];
union empty [1];

intersect[10,2,4,6,8][6,7,8,9,10];

remove  [#"A",#"B",#"C",#"D"] #"B";

val big = subset [1,2,4,5,6,9,234];
map allEven big;

val noEvens = false = setexists [1,3,5] allEven;
val noEvens = false = setall [1,3,5] allEven;

val noEvens = true = setexists [1,2,5] allEven;
val noEvens = false = setall [1,2,5] allEven;

val noEvens = true = setexists [1,2,5] allEven;
val noEvens = true = setall [8,2,54] allEven;

(subset [2,4,6]);

val sum0 = false = zeroSum [1,2,3,4,5,6,7];
val sum0 = true = zeroSum [1,2,3,4,5,6,7,~1,~2,~3,~4,~5,~6,~7];

val sumSubSet = false = subsetSum [1,2,3];
val sumSubSet = true = subsetSum [1,~1,3];

 find_subsetSum [1,~1,3];
 find_subsetSum [1,2,3];


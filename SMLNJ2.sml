(* SML comments appear like this *)
(* Your name appears here *)

(* #1 - quicksort *)
(* Function takes 2 parameters, a pivot which is initially the head of the list and then a list that is split between the head and the tail. Then the values lower and larger are*)
(* assigned to the piv and the rest of the list if the head is smaller than the pivot then the head is appended to the list 'smaller' else it is concatenated to the list 'larger' *)
fun partition(piv, nil) = (nil,nil) | partition (piv, head::rest) = let val(smaller, larger) = partition(piv, rest) in if head < piv then (head::smaller, larger) else (smaller, head::larger) end;
(* This function takes a list as a parameter. If the list is nil then nil is returned or if it only contains 1 element then it is returned because it is sorted. If it is larger than 1 element *)
(* then the list is seprated by the head and the tail and the values are assigned to the partition function call with the head and the tail. The function is recurisvely *)
(* called with the list of smaller values concatenated with the head and then the recursive call of the function which is passed the list of larger values *)
fun quicksort (nil) = nil| quicksort[x] = [x] | quicksort (head::rest) = let val(smaller,larger) = partition(head,rest) in quicksort(smaller) @ [head] @ quicksort(larger) end;

(* #2 - member *)
(* takes 2 parameters, an int and a list. Splits the list into its head and tail and recursively compares the int e to the head of the list. If it eventually compares true then true is returned, if *)
(* the int e never equals an element in the list then false is returned *)
fun member(e, nil) = false | member(e, head::rest) = e=head orelse member(e, rest); 
               
(* #3 - returns the union of sets (lists) s1 and s2*)
(* You may assume that s1 and s2 do not have any duplicate elements *)
(* Takes 2 parameters, a list and a list. Seperates the first list into its head and tail and calls the member function to see if the head is contained in the second list. If it is not then it is *)
(* appended to the recursive call of the function consisting of the tail of the first list and the entire second list *)
fun union (nil, B) = B | union (a::A, B) = if member(a,B) then union (A, B) else a::union(A,B);

(* #4 - returns the intersection of sets (lists) s1 and s2 *)
(* You may assume that s1 and s2 do not have any duplicate elements *)
(*Takes 2 parameters which are both int lists. If the first list is nil then an empty list is returned. If not then the first list is split into its head and tail and if the head is a member *)
(* of the second list then it is appended to the recursive call to the function that is passed the tail of first list and the second list, If it is not a member then the function is recursively *)
(* called with the tail of the first list and the second list being passed *)
fun intersection (nil,   B) = [] | intersection (a::A, B) = if member(a, B) then a::intersection(A,B) else intersection(A, B);

(* #5 - Return list of integers from start (inclusive) to stop (exclusive) by step *)
(* The range functoin takes 3 integer parameters. If the step parameter is greater than zero andalso the start parameter is less than the stop parameter. The start int value is appended to the *)
(* recursive call to the function that is passed the start value added to the step value, the stop value, and the same step value. If the step value is is less than zero andalso the start value is *)
(* greater than the stop value then the start value is appended to the recursive call to the function that is passed the start value added to the step value, the stop value, and step value *)
(* The base case is a empty list *)
fun range(start, stop, step) = if (step > 0 andalso start < stop) then start::range(start + step, stop, step) else if (step < 0 andalso start > stop) then start::range(start + step,stop, step) else [];

(* #6 - Return a slice of a list between indices start inclusive, and stop exclusive. Assume first element of list is at index 0*)
(* Takes 3 parameters, a list, a start value and a stop value. If the start value is greater than zero then the function is called recursively passing it the tail of the list *)
(* and the start value -1 and the stop value -1. This is the way of getting to the index of the list that the start value represents. Once the start value has reached zero the hd of the list *)
(* is appended to the recursive call of the function which is passed the tail of the list, the start value, and the stop value is decremented by 1. Once the stop value = zero the list is returned *)
fun slice(aList, start, stop) = if start > 0 then slice(tl aList, start-1, stop-1) else if (start = 0 andalso stop > 0) then hd aList::slice(tl aList, start, stop-1) else [];
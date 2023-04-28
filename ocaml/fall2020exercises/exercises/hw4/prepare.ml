let some_store_extra_add = ref false
let store_counter = ref 0

module Hashtbl = struct
	include Hashtbl

	let clear_with_ref h = 
		store_counter := 0; 
		some_store_extra_add := false;
		clear h

	let add h k v = 
		(match find_opt h k with
		| None -> ()
		| Some v -> some_store_extra_add := true
		); add h k v

	let find_opt h k = 
		store_counter := !store_counter + 1; 
		find_opt h k

end

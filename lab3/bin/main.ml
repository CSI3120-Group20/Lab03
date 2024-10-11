(* Stores location as a record type *)
type location = {
  name : string;
  x : float;
  y : float;
  priority : int;
}

(* Stores vehicle as a record type *)
type vehicle = {
  id : int;
  capacity : int;
  locations : location list;
}

(*Auxiliary function for sorting the locations by priority*)
let sort_by_priority locations = 
  List.sort(fun l1 l2 -> (compare l1.priority l2.priority)) locations;;

(* A function that prompts location details to be inputted via the command line *)
let ask_location_information location_num = 
  Printf.printf "Enter details for location %d\nLocation name: " location_num;
  let name = read_line () in 

  Printf.printf "X coordinate: ";
  let x = read_line () in

  Printf.printf "Y coordinate: ";
  let y = read_line () in
  
  Printf.printf "Priority: ";
  let priority = read_line () in

  { name = name; x = (float_of_string x); y = (float_of_string y); priority = (int_of_string priority) }


(* A function that prompts vehicle details to be inputted via the command line *)
let ask_vehicle_information vehicle_num = 
  Printf.printf "Enter details for location %d, please enter the following details:\nCapacity: " vehicle_num;
  let capacity = read_line () in 
  { id = vehicle_num; capacity = (int_of_string capacity); locations = []}
  

(*Function to calculate the distance between a location and a coordinate*)
let calc_dist x y location = 
  sqrt(((x -. location.x) *. (x -. location.x)) +. ((y -. location.y) *. (y -. location.y)))


(*Function for spliting a list of locations*)
let rec split_loc n locations = 
  (*If n is empty or the list is empty, return a tuple of empty list and original list of locations*)
  if n <= 0 || List.length(locations) == 0 then
    ([],locations)
  else
    let (head,rest) = split_loc (n-1) (List.tl locations) in (*Split the location list into head and rest*)
    (List.hd locations::head,rest) (*concatenate *)

(*Function to assign locations to vehicles based on the vehicle's capacity*)
let assign locations vehicles = 
  let sorted = sort_by_priority locations in (*Sort locations by priority*)
  (*Auxiliary function for assigning vehicles*)
  let rec assign_vehicle vehicles locations res = 
    match vehicles with
    | [] -> res (*Base case, all vehicles assigned, return resultant list*)
    | head::tail -> 
        let (assigned, remaining) = split_loc head.capacity locations in (*Split a vehicle's capacity of location into assigned, and the rest into remaining*)
        let each = {head with locations = assigned} in (*Assign the assigned locations to the current vehicle*)
        assign_vehicle tail remaining  (each :: res) in (*recurse on the reamining vehicles and locations*)
  assign_vehicle vehicles sorted [] 
    



(* A function that reads information for n locations by repeatedly calling ask_location_information n times 
  Returns a list of location records *)
let read_locations n =
  let list_locations = ref [] in
  for i = 1 to n do
    list_locations := !list_locations @ [ask_location_information i]
  done;
  !list_locations

(* A function that reads information for n vehicles by repeatedly calling ask_vehicle_information n times
  Returns a list of vehicle records *)
let read_vehicles n =
  let list_vehicles = ref [] in
  for i = 1 to n do
    list_vehicles := !list_vehicles @ [ask_vehicle_information i]
  done;
  !list_vehicles


  (*function to display the optimized route for each vehicle*)
  let print_locations_list locations =
    let dist = ref 0. in (* Iterate distance *)
    let x = ref (List.hd locations).x in (* Get x coordinate of first location in list. List.hd was suggested by ChatGPT*)
    let y = ref (List.hd locations).y in (* Get y coordinate of first location in list.*)
    List.iter (fun location ->
      dist := calc_dist !x !y location;
      Printf.printf "location: %s, distance travelled: %f\n" location.name !dist;
      x := location.x;
      y := location.y;
    ) locations 

(* Main method *)
let () =
  let prompt = "Enter the number of delivery locations: " in
  Printf.printf "%s" prompt;
  let num_locations = read_line () in
  let locations = read_locations (int_of_string num_locations) in
  print_locations_list locations;
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
}

(* 
  This recursive function prompts the user to enter a floating-point number.

  Return the floating-point number
*)
let rec get_float_number prompt_message = 
  Printf.printf "%s" prompt_message;

  try
    (* Return the float if valid *)
    read_float()
  with
  | Failure _ ->
    print_endline "Invalid input. Please enter a floating-point number (e.g. 4.0)\n";
    get_float_number prompt_message


(* 
  This recursive function prompts the user to enter a non-negative integer.

  Return the non-negative integer `int_num`
*)
let rec get_non_negative_int_number prompt_message = 
  Printf.printf "%s" prompt_message;

  try
    let int_num = read_int() in

    if int_num < 0 then(
      print_endline "Please enter a positive integer.\n";
      get_non_negative_int_number(prompt_message)
    )

    else
      (* Return the integer *)
      int_num
  with
  | Failure _ ->
    print_endline "Invalid input. Please enter an integer (e.g. 4)\n";
    get_non_negative_int_number prompt_message    

(*Auxiliary function for sorting the locations by priority*)
(* let sort_by_priority locations = 
  List.sort(fun l1 l2 -> (compare l1.priority l2.priority)) locations;; *)

(* A function that prompts location details to be inputted via the command line *)
let ask_location_information location_num = 
  Printf.printf "Enter details for location %d:\nLocation name: " location_num;
  let name = read_line() in 

  let x = get_float_number("X coordinate: ") in
  let y = get_float_number("Y coordinate: ") in
  let priority = get_non_negative_int_number("Priority: ") in

  print_endline "";

  (* Return a `location` type variable *)
  { name = name; x = x; y = y; priority = priority }


(* A function that prompts vehicle details to be inputted via the command line *)
let ask_vehicle_information vehicle_num = 
  Printf.printf "Enter details for vehicle %d:\n" vehicle_num;
  let capacity = get_non_negative_int_number("Vehicle capacity: ") in

  print_endline "";

  { id = vehicle_num; capacity = capacity;}
  
(* DEBUG *)
(*Function to calculate the distance between two points*)
(* let calc_dist loc1 loc2 = 
  sqrt((loc2.x -. loc1.x) *. (loc2.x -. loc1.x) +. (loc2.y -. loc1.y) *. (loc2.y -. loc1.y)) *)

(* DEBUG *)
(*Function for spliting a list of locations*)
(* let rec split_loc n locations = 
  (*If n is empty or the list is empty, return a tuple of empty list and original list of locations*)
  if n <= 0 || List.length(locations) == 0 then
    ([],locations)
  else
    let (head,rest) = split_loc (n-1) (List.tl locations) in (*Split the location list into head and rest*)
    (List.hd locations::head,rest) (*concatenate *)
sad *)
(*Function to assign locations to vehicles based on the vehicle's capacity*)
(* let assign locations vehicles = 
  let sorted = sort_by_priority locations in (*Sort locations by priority*)
  (*Auxiliary function for assigning vehicles*)
  let rec assign_vehicle vehicles locations res = 
    match vehicles with
    | [] -> res (*Base case, all vehicles assigned, return resultant list*)
    | head::tail -> 
        let (assigned, remaining) = split_loc head.capacity locations in (*Split a vehicle's capacity of location into assigned, and the rest into remaining*)
        let each = {head with locations = assigned} in (*Assign the assigned locations to the current vehicle*)
        assign_vehicle tail remaining  (each :: res) in (*recurse on the reamining vehicles and locations*)
  assign_vehicle vehicles sorted []  *)
    



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
    List.iter (fun location ->
      Printf.printf "name: %s name, priority: %d priority\n"
        location.name location.priority;
      Printf.printf "x: %f x, y: %f y"
      location.x location.y
    (*print out total distance here*)
    
    ) locations 

      (*function to display the optimized route for each vehicle*)
  let print_vehicle_list vehicles =
    List.iter (fun vehicle ->
      Printf.printf "id: %d, capacity: %d\n"
      vehicle.id vehicle.capacity;
    (*print out total distance here*)
    
    ) vehicles 

(* Main method *)
let () =
  let num_locations = get_non_negative_int_number("Enter the number of delivery locations: ") in
  if num_locations = 0 then(
    print_endline "You have not scheduled any deliveries.\n";

    (* Exit the program with status code 0 *)
    exit 0
  )

  else
    let locations = read_locations(num_locations) in

    let num_vehicles = get_non_negative_int_number("Enter the number of vehicles: ") in
    if num_vehicles = 0 then(
      print_endline "No vehicles are available.\n";
  
      (* Exit the program with status code 0 *)
      exit 0
    )

    else
      let vehicles = read_vehicles(num_vehicles) in

      (* DEBUG *)
      print_locations_list(locations);
      print_vehicle_list(vehicles);
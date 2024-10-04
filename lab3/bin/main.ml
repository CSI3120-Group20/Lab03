

 type location = {
  name : string;
  x : int;
  y : int;
  priority : int;
}

type vehicle = {
  id : int;
  capacity : int;
}

(*Auliary function for sorting the locations by priority*)
let sort_by_priority locations = 
  List.sort(fun l1 l2 -> (compare l1.priority l2.priority)) locations;;

let ask_location_information location_num = 
  Printf.printf "Enter details for location %d\nLocation name: " location_num;
  let name = input_line stdin in 

  Printf.printf "X coordinate: ";
  let x = input_line stdin in

  Printf.printf "Y coordinate: ";
  let y = input_line stdin in
  
  Printf.printf "Priority: ";
  let priority = input_line stdin in

  { name = name; x = (int_of_string x); y = (int_of_string y); priority = (int_of_string priority) }

let ask_vehicle_information vehicle_num = 
  Printf.printf "Enter details for location %d, please enter the following details:\nCapacity: " vehicle_num;
  let capacity = input_line stdin in 
  { id = vehicle_num; capacity = (int_of_string capacity) }
  

let read_locations num =
  let list_jobs = ref [] in
  for i = 0 to num do
    list_jobs := !list_jobs @ [ask_location_information i]
  done;
  !list_jobs

let read_vehicles num =
  let list_vehicles = ref [] in
  for i = 0 to num do
    list_vehicles := !list_vehicles @ [ask_vehicle_information i]
  done;
  !list_vehicles

(* Main method *)
let () =
  let prompt = "Enter the number of delivery locations: " in
  Printf.printf "%s" prompt;
  let num_locations = read_line () in
  let locations = read_locations (int_of_string num_locations) in
  let length = List.length locations in
  print_int length;
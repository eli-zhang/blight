open Objects
open State

let news_infected_0 =
  [
    "Speed Cubing Deemed Olympic Sport";
    "Researchers Unknowingly Run Double-Blind Experiment on Themselves";
    "Sleeping On Left Side Discovered To Be Marginally Better For Right Arm";
    "\"Any Person, Any Study,\" Changed to \"Some People, Some Studies\" as Cornell Admission Rates Drop";
    "Second Car in Space Laps First in Orbit";
    "Years of Earbud Usage Leaves Youth Debilitated For Life";
    "\"It's just a phase,\" Claims Struggling Alcoholic" 
  ]

let news_infected_20 =
  [
    "Doctors Worry About Declining Hand-Washing as Water Conservation Effort Comes To Life";
    "Bestselling Book \"Nothing to Worry About\" Tops Charts Once Again";
    "Anti-Vaccination Movement Popularizes \"Vaccines are drugs. Don't do drugs.\" Slogan";
  ]

let news_infected_40 =
  [
    "Doctors Warn About Recent Rise in Illness";
    "Effects of Social Media on Trending Decline in Personal Hygiene";
    "Proposed Six-Day Work Week Implemented Worldwide - Higher Productivity, Less Wasted Time";
  ]

let news_infected_80 =
  [
    "Widespread Sickness Hints at Troubling Future";
    "Work Productivity Drops as Millions of Employees Take Sick Days";
    "Researchers Investigating Cause of New Strain of Disease"
  ]

let news_infected_95 =
  [
    "Doctors Out of Office Due to Widespread Sickness";
    "Government Shutdown as Politicians Meet to Resolve International Crisis";
    "\"Desolate and Barren,\" Claims NYC Tourist";
    "\"I shouldn't have eaten that fish,\" Claims Alleged Origin of Worldwide Disease"
  ]

let news_dead_1 name =
  "First death due to unknown disease, named " ^ name ^ " by doctors, leads to 
  criticism of medical practices"

let news_dead_20 name =
  "Large numbers of deaths in urban areas; ties found to disease " ^ name ^ ""

let news_dead_50 name =
  "Rapidly spreading disease " ^ name ^ "takes a critical number of lives; 
  scientists doing everything they can to prevent further spread"

let news_dead_80 name =
  "Desperation sets in as humans make last-ditch effort to prevent total 
  apocalyptic failure at the hands of " ^ name

let news_dead_90 name =
  "\"Nothing left to do but wait,\" claim last survivors as " ^ name ^ " takes 
  final humans on Earth"

let random_list_ele list =
  let length = List.length list in
  let random = Random.int length in
  List.nth list random

let update_message state =
  let total_population = total_population state in
  let infected = total_infected state * 100 / total_population in
  let dead = total_dead state * 100 / total_population in
  let infected_messages =
    if infected < 20 then news_infected_0
    else if infected < 40 then news_infected_20
    else if infected < 80 then news_infected_40
    else if infected < 95 then news_infected_80
    else news_infected_95 in

  let infected_message = random_list_ele infected_messages in
  let dead_message =
    if dead = 0 then infected_message
    else if dead < 20 then news_dead_1 state.name
    else if dead < 50 then news_dead_20 state.name
    else if dead < 80 then news_dead_50 state.name
    else if dead < 90 then news_dead_80 state.name
    else if dead < 100 then news_dead_90 state.name
    else news_dead_90 state.name in

  {state with news_message = 
                if state.news_message = dead_message then infected_message
                else if state.news_message = "" then
                  infected_message 
                else if List.mem state.news_message infected_messages then
                  infected_message else dead_message}
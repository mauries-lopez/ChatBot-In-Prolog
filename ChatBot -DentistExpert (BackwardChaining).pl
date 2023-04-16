% A Backward Chaining Expert System Shell based on (Luger, 2005)
% For further details, see (Luger, 2005), section 15.7.2.

% Expert system shell based on Luger
% To run, start.

start :-
    write('[Bot: Hi, I am a ChatBot with knowledge in Dentistry.] \n'),
    write('[Bot: You can trust me for your diagnosis, I will try to be useful as ever] \n'),
    write('[Bot: Would you like to start your diagnosis? (yes/no)]: '),
    read(Answer),
    (
        helper_answer(Answer) ->
           write('[Bot: Are you in an emergency situation? (yes/no)]: '),
           read(Emergency),

           (   helper_answer(Emergency)
               ->
                  (   solve(fix(X), CF),
                  write('\n[Bot: Due to emergency reason/s, I will provide POSSIBLE OTC treatment/s.]'),
                  find_dental_problem(X, Dental_problem),
                  replace_treatment(X, OTC_treatment),
                  write('\n[Diagnosis: '), write(Dental_problem), write(']'),
                  write('\n[Bot Certainty: '), write(CF), write(']'),
                  write('\n[OTC Treatments: '), write(OTC_treatment), write(']')  )
                ;
                  (   solve(fix(X), CF),
                  find_dental_problem(X, Problem),
                  write('\n[Diagnosis: '), write(Problem), write(']'),
                  write('\n[Bot Certainty: '), write(CF), write(']') ),
                  write('\n[Bot: For further accurate diagnosis. Please proceed to your nearest dentist]'),
                  write('\n[Bot: Dentist will use instruments to diagnose your problem.]'),
                  write('\n[Bot: Also, provides instruction/s to treat your dental problem]'),
                  write('\n[Possible Treatment/s: '), write(X), write(']') )
        ;write('Have a great day ahead. Remember, take care of your dental health!'),
         true
     ).

helper_answer(Answer) :-
    member(Answer, [yes, y]).

find_dental_problem(Treatments, X) :-
    findall(Problem, rule(fix(Problem, Treatments), _), Problems),
    member(Dental_problem, Problems),
    string_upper(Dental_problem, X).

% solve(+,?)
solve(Goal,CF) :-
   print_instructions,
   retractall(known(_,_)),
   solve(Goal,CF,[],20).

print_instructions :-
   nl, write('[Bot: You will be asked a series of queries. ]'), nl,
   write('[Bot: Your response MUST BE either: ]'), nl, nl,
   write('1.) - - - A number between (-100 and 100) representing'), nl,
   write('          your confidence in the truth of the query'), nl, nl,
   write('2.) - - - why'), nl, nl,
   write('3.) - - - how(X), where X is a goal. (Ex. how(cavity))'),nl.

% solve(+,?,+,+)
solve(Goal,CF,_,Threshold) :-
   known(Goal,CF),!,above_threshold(CF,Threshold).

solve(not(Goal),CF,Rules,Threshold) :- !,
   invert_threshold(Threshold,New_threshold),
   solve(Goal,CF_goal,Rules,New_threshold),
   negate_cf(CF_goal,CF).

solve((Goal1,Goal2),CF,Rules,Threshold) :- !,
   solve(Goal1,CF1,Rules,Threshold),
   above_threshold(CF1,Threshold),
   solve(Goal2,CF2,Rules,Threshold),
   above_threshold(CF2,Threshold),
   and_cf(CF1,CF2,CF).

solve(Goal,CF,Rules,Threshold) :-
   rule((Goal:-(Premise)),CF_rule),
   solve(Premise,CF_premise,[rule((Goal:-Premise),CF_rule)|Rules],Threshold),rule_cf(CF_rule,CF_premise,CF),
   above_threshold(CF,Threshold).

solve(Goal,CF,_,Threshold) :-
   rule(Goal,CF),
   above_threshold(CF,Threshold).

solve(Goal,CF,Rules,Threshold) :-
   askable(Goal),
   askuser(Goal,CF,Rules),!,
   assert(known(Goal,CF)),
   above_threshold(CF,Threshold).

above_threshold(CF,T) :- T>=0, CF>=T.
above_threshold(CF,T) :- T<0, CF=<T.

invert_threshold(Threshold,New_threshold) :-
   New_threshold is -1 * Threshold.

negate_cf(CF,Negated_CF) :-
   Negated_CF is -1 * CF.

and_cf(A,B,A) :- A =< B.
and_cf(A,B,B) :- B < A.
rule_cf(CF_rule,CF_premise,CF) :-
   CF is (CF_rule * CF_premise / 100).

% askuser(+,?,+)
askuser(Goal,CF,Rules) :-
   nl,write('Symptom: '),
   write(Goal), write('?'),
   read(Ans),
   respond(Ans,Goal,CF,Rules).

% respond(+,+,?,+)
respond(CF,_,CF,_) :-
   number(CF), CF=<100, CF>= -100. % no response issued because user enters a valid CF
respond(why,Goal,CF,[Rule|Rules]) :-
   nl, write_rule(Rule),
   askuser(Goal,CF,Rules).
respond(why,Goal,CF,[]) :-
   nl, write('Back to top of rule stack.'), nl,
   askuser(Goal,CF,[]).
respond(how(X),Goal,CF,Rules) :-
   build_proof(X,CF_X,Proof), !,
   nl, write('The goal '), write(X),
   write(' was concluded with certainty '),
   write(CF_X), write('.'), nl, nl, write('The proof of this is:'), nl,
   write_proof(Proof,0), nl,
   askuser(Goal,CF,Rules).
respond(how(X),Goal,CF,Rules) :-
   write('The truth of '), write(X), nl,write('is not yet known.'), nl,
   askuser(Goal,CF,Rules).
respond(_,Goal,CF,Rules):-
   write_rule('Unrecognized response.'), nl,
   askuser(Goal,CF,Rules).

% build_proof(+,?,?)
build_proof(Goal,CF,(Goal,CF:-given)) :-
   known(Goal,CF), !.
build_proof(not(Goal),CF,not(Proof)) :- !,
   build_proof(Goal,CF_goal,Proof), negate_cf(CF_goal,CF).
build_proof((Goal1,Goal2),CF,(Proof1,Proof2)) :-
   build_proof(Goal1,CF1,Proof1),
   build_proof(Goal2,CF2,Proof2), and_cf(CF1,CF2,CF).
build_proof(Goal,CF,(Goal,CF:-Proof)) :-
   rule((Goal:-Premise),CF_rule),
   build_proof(Premise,CF_premise,Proof),
   rule_cf(CF_rule,CF_premise,CF).
build_proof(Goal,CF,(Goal,CF:-fact)) :- rule(Goal,CF).

write_rule(rule((Goal:-(Premise)),CF)) :-
   write('I am trying to prove the following rule:'), nl, write(Goal),
   write(':-'), nl,
   write_premise(Premise),
   write('CF = '), write(CF), nl.
write_rule(rule(Goal,CF)) :-
   write('I am trying to prove the following goal:'), nl,
   write(Goal),
   write('CF = '), write(CF), nl.

write_premise((Premise1,Premise2)) :- !,
   write_premise(Premise1),
   write_premise(Premise2).
write_premise(not(Premise)) :- !,
   write(' '), write(not), write(' '), write(Premise), nl.
write_premise(Premise) :- !,
   write(' '), write(Premise), nl.

% write_proof(+,+)
write_proof((Goal,CF:-given),Level) :-
   indent(Level), write(Goal),
   write(' CF='), write(CF),
   write(' was given by the user'), nl, !.
write_proof((Goal,CF:-fact),Level) :-
   indent(Level), write(Goal),
   write(' CF='), write(CF),
   write(' was a fact in the KB'), nl, !.
write_proof((Goal,CF:-Proof),Level) :-
   indent(Level), write(Goal),
   write(' CF='), write(CF),
   write(':-'), nl,
   New_level is Level + 1,
   write_proof(Proof,New_level), !.
write_proof(not(Proof),Level) :-
   indent(Level), write((not)), nl,
   New_level is Level + 1,
   write_proof(Proof,New_level), !.
write_proof((Proof1,Proof2),Level) :-
   write_proof(Proof1,Level),
   write_proof(Proof2,Level), !.

indent(0).
indent(X) :-
   write(' '), X_new is X - 1, indent(X_new).


replace_treatment('(1)Fillings, (2)Crowns', X) :- %Cavity
    X = '(1)Non-steroidal anti-inflammatory drugs(To relieve pain)'.
replace_treatment('(1)Dental Health Education, (2)Oral Hygiene, (3)Frequent Oral Prophylaxis', X) :- %Periodontal Disease
    X = '(1)Toothpaste contains Triclosan, (2)Antimicrobial Mouthwash'.
replace_treatment('(1)Professional Dental Cleaning, (2)Dental Restoration, (3)Good Oral Hyigene', X) :- %Gingivitis
    X = '(1)Toothpaste contains Triclosan, (2)Antimicrobial Mouthwash'.
replace_treatment('(1)Topical Anitbiotics, (2)Dental Bonding, (3)Orthodontics', X) :- %Gum Recession
    X = 'N/A'.
replace_treatment('(1)Root Canal, (2)Pull the affected tooth, (3)Prescribe antibiotics', X) :- %Tooth Infection
    X = '(1)Anti-inflammatory drugs, (2) Cold Compress'.
replace_treatment('(1)Good Oral Hygiene', X) :- %Halitosis
    X = '(1)Anti-bacterial toothpaste and mouthwash'.
replace_treatment('(1)Apply moist heat or cold packs, (2)Eat soft foods, (3)Wear splint or night guard', X) :- %TMJ Problem
    X = '(1)Apply moist heat or cold packs, (2)Eat soft foods, (3)Wear splint or night guard'.
replace_treatment('(1)Use desensitizing toothpaste when brushing your teeth, (2)Use softer toothbrushes, (3)Root canal', X) :- %Sensitive Teeth
    X = '(1)Use desensitizing toothpaste when brushing your teeth, (2)Use softer toothbrushes'.
replace_treatment('(1)Tooth bonding, (2)Crowns', X) :- %Enamel Erosion
    X = 'N/A'.
replace_treatment('(1)Wear splints or mouth guard, (2)Dental correction', X) :- %Bruxism
    X = '(1)Wear splints or mouth guard'.

rule((fix(Advice):- dental_problem(X),fix(X,Advice)),100).

rule((dental_problem(cavity):- (symptom(spots_teeth),symptom(hole_teeth))),80).
rule((dental_problem(periodontal_disease):-(symptom(bone_loss),symptom(gum_inflammation))),80).
rule((dental_problem(gingivitis):-(symptom(gum_inflammation),symptom(gums_bleed_easily))),80).
rule((dental_problem(gum_recession):-(symptom(tooth_root_exposure),symptom(discomfort_near_gum_line))),80).
rule((dental_problem(tooth_infection):-(symptom(tooth_ache),symptom(swelling_in_face))),80). %SEVERE
rule((dental_problem(halitosis):-(symptom(bad_breath),symptom(unpleasant_taste))),80).
rule((dental_problem(tmj_problem):-(symptom(jaw_pain),symptom(difficulty_opening_mouth))),80). %SEVERE
rule((dental_problem(sensitive_teeth):-(symptom(tooth_sensitive),symptom(pain_during_brushing_flossing))),80).
rule((dental_problem(enamel_erosion):-(symptom(tooth_sensitive),symptom(shiny_surface_on_teeth))),80).
rule((dental_problem(bruxism):-(symptom(teeth_grinding),symptom(teeth_flattened_or_outofshape))),80).

rule((symptom(spots_teeth) :- (brown_black_stain_tooth)), 80).
rule((symptom(hole_teeth):- (visible_hole_in_tooth)), 50).
rule((symptom(bone_loss) :- (teeth_thinning_out)), 50).
rule((symptom(gum_inflammation) :- (gums_are_swelling)),80).
rule((symptom(gum_bleed_easily) :- (gums_bleed_when_brushing_flossing)), 70).
rule((symptom(tooth_root_exposure) :- (tooth_root_is_exposed)), 80).
rule((symptom(discomfort_near_gum_line) :- (gums_are_swelling)), 50).
rule((symptom(tooth_ache) :- (feeling_intense_pressure_in_teeth)), 70).
rule((symptom(swelling_in_face) :- (swelling_in_face)), 50).
rule((symptom(bad_breath) :- (bad_smelly_breath)),100).
rule((symptom(unpleasant_taste) :- (uncomfortable_tastes_from_substance)), 80).
rule((symptom(jaw_pain) :- (pressure_in_jaw)), 80).
rule((symptom(difficulty_opening_mouth) :- (unable_to_open_mouth_fully)), 70).
rule((symptom(tooth_sensitive) :- (sensitive_with_hot_cold_sweet_substance)), 60).
rule((symptom(pain_during_brushing_flossing) :- (pain_during_brushing_flossing)), 90).
rule((symptom(shiny_surface_on_teeth):- (shiny_smooth_part_on_teeth)), 90).
rule((symptom(teeth_grinding) :- (disrupted_sleep)), 60).
rule((symptom(teeth_flattened_or_outofshape) :- (teeth_flattened_or_outofshape)), 90).

rule(fix(cavity,'(1)Fillings, (2)Crowns'),100).
rule(fix(periodontal_disease,'(1)Dental Health Education, (2)Oral Hygiene, (3)Frequent Oral Prophylaxis'),100).
rule(fix(gingivitis,'(1)Professional Dental Cleaning, (2)Dental Restoration, (3)Good Oral Hyigene'),100).
rule(fix(gum_recession,'(1)Topical Anitbiotics, (2)Dental Bonding, (3)Orthodontics'),100).
rule(fix(tooth_infection,'(1)Root Canal, (2)Pull the affected tooth, (3)Prescribe antibiotics'),100).
rule(fix(halitosis, '(1)Good Oral Hygiene'), 100).
rule(fix(tmj_problem, '(1)Apply moist heat or cold packs, (2)Eat soft foods, (3)Wear splint or night guard'), 100).
rule(fix(sensitive_teeth, '(1)Use desensitizing toothpaste when brushing your teeth, (2)Use softer toothbrushes, (3)Root canal'), 100).
rule(fix(enamel_erosion, '(1)Tooth bonding, (2)Crowns'), 100).
rule(fix(bruxism, '(1)Wear splints or mouth guard, (2)Dental correction'), 100).

askable(brown_black_stain_tooth). %spots_teeth
askable(visible_hole_in_tooth). %hole_teeth
askable(teeth_thinning_out). %bone_loss
askable(gums_are_swelling). %gum_inflammation
askable(gums_bleed_when_brushing_flossing). %gum_bleed_easily
askable(tooth_root_is_exposed). %tooth_root_exposure
askable(discomfort_near_gum_line). %discomfort_near_gum_line
askable(feeling_intense_pressure_in_teeth). %tooth_ache
askable(swelling_in_face). %swelling_in_face
askable(bad_smelly_breath). %bad_breath
askable(uncomfortable_tastes_from_substance). %unpleasant_taste
askable(pressure_in_jaw). %jaw_pain
askable(unable_to_open_mouth_fully). %difficulty_opening_mouth
askable(sensitive_with_hot_cold_sweet_substance). %tooth_sensitive
askable(pain_during_brushing_flossing). %pain_during_brushing_flossing
askable(shiny_smooth_part_on_teeth). %shiny_surface_on_teeth
askable(disrupted_sleep). %teeth_grinding
askable(teeth_flattened_or_outofshape). %teeth_flattened_or_outofshape
askable(poking_like_feeling_on_teeth). %sharp_pain

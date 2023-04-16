% Dental Problems %
dental_problem(cavity).
dental_problem(periodontal_disease).
dental_problem(gingivitis).
dental_problem(gum_recession).
dental_problem(tooth_infection).
dental_problem(tmj_problem).
dental_problem(sensitive_teeth).
dental_problem(halitosis).
dental_problem(enamel_erosion).
dental_problem(bruxism).

% Fact Sub symptoms ( Value is always upto 100 )
% Legends: 1 - 1st Major Symptom, 2 - 2nd Major Symptom (POSSIBLE
% UNIQUE)
% The LOWER the VALUE is, the COMMON it is from other dental
% problems.
% The HIGHER the VALUE is, the UNIQUE it is from other dental
% problems.
%
% 1st Major Symptom value is 40. Never be 50 above because it needs to
% consider other symptoms as well.
%
% 2nd Major Symptom value is 30. Same reason with 1st major symptom
% value.
symptom(cavity, tooth_sensitive, 5).
symptom(cavity, sharp_pain, 7.5).
symptom(cavity, spots_teeth, 30). % 2
symptom(cavity, hole_teeth, 40). % 1
symptom(cavity, bad_breath, 2.5).
symptom(cavity, tooth_ache, 7.5).
symptom(cavity, unpleasant_taste, 2.5).
symptom(cavity, gum_inflammation, 5).
symptom(periodontal_disease, gum_inflammation, 33.33). % 2
symptom(periodontal_disease, receding_gums, 16.665).
symptom(periodontal_disease, bone_loss, 49.995). % 1
symptom(gingivitis, gum_inflammation,  40). % 1
symptom(gingivitis, receding_gums, 3.75).
symptom(gingivitis, dark_red_gums, 11.25).
symptom(gingivitis, gums_bleed_easily, 30). % 2
symptom(gingivitis, bad_breath, 3.75).
symptom(gingivitis, tender_gums, 11.25).
symptom(gum_recession, tooth_sensitive, 2.5).
symptom(gum_recession, tooth_root_exposure, 40). % 1
symptom(gum_recession, discomfort_near_gum_line, 30). % 2
symptom(gum_recession, sensitive_when_brushing, 13.75).
symptom(gum_recession, sensitive_when_dental_cleaning, 13.75).
symptom(tooth_infection, tooth_ache, 40). %1
symptom(tooth_infection, bad_breath, 2.5).
symptom(tooth_infection, pain_when_biting, 5).
symptom(tooth_infection, discomfort_with_temperatures, 5).
symptom(tooth_infection, fever, 7.5).
symptom(tooth_infection, swelling_in_face, 30). % 2
symptom(tooth_infection, swollen_lymph_nodes_under_jaw, 5).
symptom(tooth_infection, foul_smelling_tasting, 5).
symptom(halitosis, bad_breath, 60). % 1
symptom(halitosis, unpleasant_taste, 40). % 2
symptom(tmj_problem, sharp_pain, 7.5).
symptom(tmj_problem, jaw_pain, 40). % 1
symptom(tmj_problem, head_ache, 7.5).
symptom(tmj_problem, difficulty_opening_mouth, 30). % 2
symptom(tmj_problem, difficulty_chewing, 7.5).
symptom(tmj_problem, popping_jaw_joint_when_opening_mouth, 7.5).
symptom(sensitive_teeth, tooth_sensitive_in_hot_cold, 40). % 1
symptom(sensitive_teeth, pain_during_brushing, 30). % 2
symptom(sensitive_teeth, sensitive_to_acidic_sweet_substance, 30).
symptom(enamel_erosion, tooth_sensitive, 20). % 2
symptom(enamel_erosion, tooth_discoloration, 30).
symptom(enamel_erosion, shiny_surface_on_teeth, 50). % 1
symptom(bruxism, tooth_sensitive, 1).
symptom(bruxism, sharp_pain, 1).
symptom(bruxism, teeth_grinding, 40). % 1
symptom(bruxism, teeth_flattened_or_outofshape, 30). % 2
symptom(bruxism, tired_tight_jaw_muscles, 5).
symptom(bruxism, pain_like_earache, 5).
symptom(bruxism, worn_tooth_enamel, 5).
symptom(bruxism, sharp_pain, 1).
symptom(bruxism, dull_headache, 3).
symptom(bruxism, sleep_disruption, 3).

% Dental Problem Possible Treament %
dental_treatment(cavity, ["(1)Fillings", "(2)Crowns"]).
dental_treatment(periodontal_disease, ["(1)Dental Health Education", "(2)Oral Hygiene", "(3)Frequent Oral Prophylaxis"]).
dental_treatment(gingivitis, ["(1)Professional Dental Cleaning", "(2)Dental Restoration", "(3)Good Oral Hygiene"]).
dental_treatment(gum_recession, ["(1)Topical Antibiotics", "(2)Dental Bonding", "(3)Orthodontics"]).
dental_treatment(tooth_infection, ["(1)Root Canal", "(2)Pull the Affected Tooth", "(3)Prescribe Antibiotics"]).
dental_treatment(halitosis, ["(1)Good Oral Hygiene"]).
dental_treatment(tmj_problem, ["(1)Apply moist heat or cold packs", "(2)Eat Soft Foods", "(3)Wear Splint or Night Guard"]).
dental_treatment(sensitive_teeth,["(1)Use Desensitizing Toothpaste when brushing your teeth", "(2)Use softer toothbrushes", "(3)Root Canal"]).
dental_treatment(enamel_erosion, ["(1)Tooth Bonding", "(2)Crowns"]).
dental_treatment(bruxism, ["(1)Wear Splints or Mouth Guard", "(2)Dental Correction"]).

% Main_Rule for assuring correct diagnosis by knowing if there are
% present major symptoms in the patient or atleast one major symptom.
%
% Sub_Rule for reliability of the bot
main_rule(dental_problem(cavity), FoundSymptoms) :-
    (member(hole_teeth, FoundSymptoms),member(spots_teeth, FoundSymptoms));
    (member(hole_teeth, FoundSymptoms);member(spots_teeth, FoundSymptoms)).
main_rule(dental_problem(periodontal_disease), FoundSymptoms) :-
    (member(bone_loss, FoundSymptoms),member(gum_inflammation, FoundSymptoms));
    (member(bone_loss, FoundSymptoms);member(gum_inflammation, FoundSymptoms)).
main_rule(dental_problem(gingivitis), FoundSymptoms) :-
    (member(gum_inflammation, FoundSymptoms),member(gums_bleed_easily, FoundSymptoms));
    (member(gum_inflammation, FoundSymptoms);member(gums_bleed_easily, FoundSymptoms)).
main_rule(dental_problem(gum_recession), FoundSymptoms) :-
    (member(tooth_root_exposure, FoundSymptoms),member(discomfort_near_gum_line, FoundSymptoms));
    (member(tooth_root_exposure, FoundSymptoms);member(discomfort_near_gum_line, FoundSymptoms)).
main_rule(dental_problem(tooth_infection), FoundSymptoms) :-
    (member(tooth_ache, FoundSymptoms),member(swelling_in_face, FoundSymptoms));
    (member(tooth_ache, FoundSymptoms);member(swelling_in_face, FoundSymptoms)).
main_rule(dental_problem(halitosis), FoundSymptoms) :-
    (member(bad_breath, FoundSymptoms),member(unpleasant_taste, FoundSymptoms));
    (member(bad_breath, FoundSymptoms);member(unpleasant_taste, FoundSymptoms)).
main_rule(dental_problem(tmj_problem), FoundSymptoms) :-
    (member(jaw_pain, FoundSymptoms),member(difficulty_opening_mouth, FoundSymptoms));
    (member(jaw_pain, FoundSymptoms);member(difficulty_opening_mouth, FoundSymptoms)).
main_rule(dental_problem(sensitive_teeth), FoundSymptoms) :-
    (member(tooth_sensitive_in_hot_cold, FoundSymptoms),member(pain_during_brushing, FoundSymptoms));
    (member(tooth_sensitive_in_hot_cold, FoundSymptoms);member(pain_during_brushing, FoundSymptoms)).
main_rule(dental_problem(enamel_erosion), FoundSymptoms) :-
    (member(tooth_sensitive, FoundSymptoms),member(shiny_surface_on_teeth, FoundSymptoms));
    (member(tooth_sensitive, FoundSymptoms);member(shiny_surface_on_teeth, FoundSymptoms)).
main_rule(dental_problem(bruxism), FoundSymptoms) :-
    (member(teeth_grinding, FoundSymptoms), member(teeth_flattened_or_outofshape, FoundSymptoms));
    (member(teeth_grinding, FoundSymptoms); member(teeth_flattened_or_outofshape, FoundSymptoms)).
sub_rule(dental_problem(cavity), Certainty_level, Bot_confidence, FoundSymptoms) :-
    (Certainty_level == 0, Bot_confidence = 90);
    (Certainty_level < 70, Bot_confidence = 50);
    (Certainty_level >= 70, Bot_confidence = 90, main_rule(dental_problem(cavity),FoundSymptoms)).
sub_rule(dental_problem(periodontal_disease), Certainty_level, Bot_confidence, FoundSymptoms) :-
    (Certainty_level == 0, Bot_confidence = 90);
    (Certainty_level < 70, Bot_confidence = 50);
    (Certainty_level >= 70, Bot_confidence = 90, main_rule(dental_problem(periodontal_disease),FoundSymptoms)).
sub_rule(dental_problem(gingivitis), Certainty_level, Bot_confidence, FoundSymptoms) :-
    (Certainty_level == 0, Bot_confidence = 90);
    (Certainty_level < 70, Bot_confidence = 50);
    (Certainty_level >= 70, Bot_confidence = 90, main_rule(dental_problem(gingivitis),FoundSymptoms)).
sub_rule(dental_problem(receding_gums), Certainty_level, Bot_confidence, FoundSymptoms) :-
    (Certainty_level == 0, Bot_confidence = 90);
    (Certainty_level < 70, Bot_confidence = 50);
    (Certainty_level >= 70, Bot_confidence = 90, main_rule(dental_problem(receding_gums),FoundSymptoms)).
sub_rule(dental_problem(tooth_infection), Certainty_level, Bot_confidence, FoundSymptoms) :-
    (Certainty_level == 0, Bot_confidence = 90);
    (Certainty_level < 70, Bot_confidence = 50);
    (Certainty_level >= 70, Bot_confidence = 90, main_rule(dental_problem(tooth_infection),FoundSymptoms)).
sub_rule(dental_problem(tmj_problem), Certainty_level, Bot_confidence, FoundSymptoms) :-
    (Certainty_level == 0, Bot_confidence = 90);
    (Certainty_level < 70, Bot_confidence = 50);
    (Certainty_level >= 70, Bot_confidence = 90, main_rule(dental_problem(tmj_problem),FoundSymptoms)).
sub_rule(dental_problem(halitosis), Certainty_level, Bot_confidence, FoundSymptoms) :-
    (Certainty_level == 0, Bot_confidence = 90);
    (Certainty_level < 70, Bot_confidence = 50);
    (Certainty_level >= 70, Bot_confidence = 90, main_rule(dental_problem(halitosis),FoundSymptoms)).
sub_rule(dental_problem(enamel_erosion), Certainty_level, Bot_confidence, FoundSymptoms) :-
    (Certainty_level == 0, Bot_confidence = 90);
    (Certainty_level < 70, Bot_confidence = 50);
    (Certainty_level >= 70, Bot_confidence = 90, main_rule(dental_problem(enamel_erosion),FoundSymptoms)).
sub_rule(dental_problem(bruxism), Certainty_level, Bot_confidence, FoundSymptoms) :-
    (Certainty_level == 0, Bot_confidence = 90);
    (Certainty_level < 70, Bot_confidence = 50);
    (Certainty_level >= 70, Bot_confidence = 90, main_rule(dental_problem(bruxism),FoundSymptoms)).

write_dental_problems :-
    write('[Write in #lowercase#, #case sensitive#, and #character sensitive#] '),
    write('\n[Copy exact text]'),
    write('\n[Ex. tooth_infection]\n'),
    write('\n1.) cavity - - - - - - - - [A hole in teeth],[Yellow or Dark spots in teeth]\n'),
    write('\n2.) periodontal_disease  - [Bone loss], [Gum Inflammation]\n'),
    write('\n3.) gingivitis - - - - - - [Gum Inflammation], [Gums bleed easily from brushing or flossing]\n'),
    write('\n4.) gum_recession  - - - - [Root of the tooth is exposed], [Discomfort can be felt near gum line]\n'),
    write('\n5.) tooth_infection  - - - [Tooth Ache], [Swelling in the face]\n'),
    write('\n6.) tmj_problem  - - - - - [Jaw pain], [Difficulty in opening mouth]\n'),
    write('\n7.) halitosis  - - - - - - [Bad breath], [Unpleasant taste]\n'),
    write('\n8.) enamel_erosion - - - - [Shiny surface on teeth], [Tooth is sensitive to cold/hot/sweet substance]\n'),
    write('\n9.) bruxism  - - - - - - - [Teeth grinding], [Teeth is flattened/chopped/crooked/out of shape]\n'), nl.

diagnose_problem(Dental_problem, Emergency) :-
    dental_problem(Dental_problem),
    ask_symptoms(Dental_problem, Emergency).

ask_symptoms(Dental_problem, Emergency) :-
    findall(Symptom, has_symptom(Dental_problem, Symptom), Symptoms),
    string_upper(Dental_problem, X),
    write('\n[Bot: Do you have any of the following symptoms for '), write(X), write('?]'), nl,
    read_symptoms(Dental_problem, Symptoms, [], Emergency).

read_symptoms(Dental_problem, [], Symptom_list, Emergency) :-
    process_symptoms(Dental_problem, Symptom_list, Emergency).
read_symptoms(Dental_problem, [Symptom|RestSymptoms], SymptomList, Emergency) :-
    write('Do you have the symptom '), write(Symptom), write('? (y/n)'), nl,
    read(Answer),
    ( Answer = y ->
        NewSymptomList = [Symptom|SymptomList];
      Answer = n ->
        NewSymptomList = SymptomList
    ),
    read_symptoms(Dental_problem, RestSymptoms, NewSymptomList, Emergency).


process_symptoms(Dental_problem, Symptom_list, Emergency) :-
    findall(Symptom, (member(Symptom, Symptom_list), has_symptom(Dental_problem, Symptom)), FoundSymptoms),
    compute_threat_level(Dental_problem, Symptom_list, Certainty_level),
    process_diagnosis(Dental_problem, Certainty_level, FoundSymptoms, Emergency).

compute_threat_level(Dental_problem, Symptom_list, Certainty_level) :-
    findall(Symptom, (member(Symptom, Symptom_list), has_symptom(Dental_problem, Symptom)), FoundSymptoms),
    findall(FoundSymptom_value, (member(Symptom, FoundSymptoms), symptom(Dental_problem, Symptom, FoundSymptom_value)), FoundSymptom_values),
    sum_list(FoundSymptom_values, FoundSymptom_total),
    Certainty_level is round(FoundSymptom_total).


process_diagnosis(Dental_problem, Certainty_level, FoundSymptoms, Emergency) :-
    sub_rule(dental_problem(Dental_problem), Certainty_level, Bot_confidence, FoundSymptoms),
    string_upper(Dental_problem, X),
    dental_treatment(Dental_problem, Treatments),
    write('\n[Bot Confidence: '), write(Bot_confidence), write('%/100%.]'),
    write('\n[Certainty Level: '), write(Certainty_level), write('%/100%.]'),
    (
        ( Bot_confidence == 90, Certainty_level == 0)
         ->
          write('\n[Diagnosis: It is unlikely you have '), write(X), write('.]\n'),
          begin_chat(Emergency)
         ;
        ( Bot_confidence =< 50, Certainty_level =< 70 ) ->
          continue_diagnosis(Dental_problem, FoundSymptoms)
         ;
            write('\n[Diagnosis: Based on your symptoms, I suspect that you may have '), write(X), write('.]'),
          ( Emergency == yes )
              ->
              write('\n[Bot: Due for emergency reason/s, I will provide possible treatments you can do.]'),
              format('\n[Treatment: Possible treatment for ~w: ~w~n', [Dental_problem, Treatments])
              ;
              write('\n[Diagnosis: Please, refer to your nearest dentist for further diagnosis.]'),
              write('\n[Treatment: Dentist will be providing further instruction/s on how to treat your dental problem.]')

    ).

continue_diagnosis(Dental_problem, FoundSymptoms) :-
    string_upper(Dental_problem, X),
    ( main_rule(dental_problem(Dental_problem), FoundSymptoms)
      ->
          print_found_symptoms(Dental_problem, FoundSymptoms, Found_MajorSymptom),
          write('\n[Bot: It is possible that you have ['), write(X), write('] because a major symptom is found!.'),
          write('\n[Major Symptom Found: '), write(Found_MajorSymptom), write('].')
         ;
          write('\n[Diagnosis: It is unlikely you have '), write(X), write('.]\n')
    ).

print_found_symptoms(Dental_problem, FoundSymptoms, Found_MajorSymptom) :-
    main_rule(dental_problem(Dental_problem), X),
    member(Symptom, X),
    member(Symptom, FoundSymptoms),
    Found_MajorSymptom = Symptom.


has_symptom(Dental_problem, Symptom) :-
    symptom(Dental_problem,Symptom, _).

% Helper Rules %
helper_answer(Answer) :-
    member(Answer, [yes, y]).

begin_chat(Emergency) :-
    write('\n[Bot: Choose one dental problem (1) you might think you have currently]'),
    write('\n[Bot: I will diagnose your dental problem basing on your symptoms]\n\n'),
    write_dental_problems,
    read(Patient_problem),
    diagnose_problem(Patient_problem, Emergency).


start :-
    write('[Bot: Hi, I am a ChatBot with knowledge in Dentistry.] \n'),
    write('[Bot: You can trust me for your diagnosis, I will try to be useful as ever] \n'),
    write('[Bot: Would you like to start your diagnosis? (yes/no)]: \n'),
    read(Answer),
    (
        helper_answer(Answer) ->
           write('[Bot: Are you in an emergency situation? (yes/no)]: \n'),
           read(Emergency),

           (   helper_answer(Emergency) ->
               begin_chat(Emergency)
               ;begin_chat(Emergency) )

        ;write('Have a great day ahead. Remember, take care of your dental health!')
     ).


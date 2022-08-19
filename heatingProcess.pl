/* 冷却方式 */
cooling_type(oil).                                          /* 油冷 */
cooling_type(water).                                        /* 水冷 */
cooling_type(furnace).                                      /* 炉冷 */
cooling_type(air).                                          /* 空冷 */

/* 热处理方式 */
treatment_type(tempering).                                  /* 回火 */
treatment_type(hardening).                                  /* 淬火 */
treatment_type(annealing).                                  /* 退火 */
treatment_type(spheroidizing_annealing).                    /* 球化退火 */
treatment_type(aging).                                      /* 时效处理 */

/* 热处理炉 */
furnace_type(tempering_furnace).                            /* 回火炉 */
furnace_type(hardening_furnace).                            /* 淬火炉 */
furnace_type(annealing_furnace).                            /* 退火炉 */

/* 可控制设备 */
% feeding_platform(Slots).                                    /* 上料台 */

/* 任务 - 不可打断的中断的连续操作序列 */
/* takeing_sample_to_heat(Slot, Furnace, Duration).
heating(Furnace, HeatingCurve, Duration).
preparing_to_cool(Furnac, Duration).
oil_cooling(Furnac, CoolingDuration, Duration). */

/* appending_slot料位 */

/* 上料台 */

% fsm_process(s_idle, to_schedule)
/* 工艺路线 */
% heat_treating(TreatmentType, Temperature, SoakingTime, CoolingType).        /* 热处理(热处理方式，加热温度, 保温时间, 冷却方式) */
% routing(Seq, Name, HeatTreatings).                                          /* 工艺路线（编号，名称，热处理列表）*/
% predefined_routings(Routings).                                              /* 预定义工艺路线 */

routing(1, 'r1', ht1).
routing(2, 'r2', ht2).
routing(3, 'r3', ht3).


% routing_patten(Routes).                              /* 工艺路线包括多个阶段 */
/* 热处理 */
/* process :- 

    routing(Routine, Name, HeatTreatings),
    format(Name),
    format(HeatTreatings).
    %!.

can
 */

/* power_on :-
   write("execute power_on"),
   nl.

power_off :-
   write("execute power_off"),
   nl.

reset :-
   write("execute reset"),
   nl.



starting :-
   power_on,
   reset.

s_system_idel :- 
   write("running ........"),
   nl,
   power_off.

run(Input_file) :-
   power_on,
   reset,
   s_system_idel.

s_slot_on(N) :-
   slot_loaded(N).

system_toProcess(Id, Sample) :-
   slot_ready(Id, Sample).
 */

/* '油冷'(Duration).
'淬火'(Temperature, HeatingDuration, CoolingType).
'工艺路线'([]).
'工艺路线'(b, 2).*/


main :-
   greeting,
   repeat,
   write('> '),
   read(X),
   do(X),
   X == quit.

greeting :-
   write('This is the native Prolog shell.'), nl.

do(quit).

do(X) :-
   write(X),
   write(' is not a legal command.'), nl,
   fail.

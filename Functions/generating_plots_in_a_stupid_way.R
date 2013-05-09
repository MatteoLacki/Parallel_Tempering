	# Wanna see it? Why not !
png(filename="/home/matteo/Documents/Scienza/Laurea_di_Matematica/Biology_Group_Seminar/picts/Liang_perspective.png", width=800,height=800)
	LIANG_PERSPECTIVE(30,40)
dev.off()

png(filename="/home/matteo/Documents/Scienza/Laurea_di_Matematica/Biology_Group_Seminar/picts/Liang_perspectives.png", width=1600,height=1600)
	LIANG_PERSPECTIVES(30,40)
dev.off()

png(filename="/home/matteo/Documents/Scienza/Laurea_di_Matematica/Biology_Group_Seminar/picts/Liang_Contour_plot.png", width=800,height=800)
	LIANG_CONTOUR_PLOT()
dev.off()

png(filename="/home/matteo/Documents/Scienza/Laurea_di_Matematica/Biology_Group_Seminar/picts/Liang_Contour_plots.png", width=800,height=800)
	LIANG_CONTOUR_PLOTS()
dev.off()

#############################################################
#Simulation

source("./Functions/ploting.R")
ls()
	# Original Metropolis-Hastings
mh_simulation_plot <- METROPOLIS_HASTINGS_PLOT(10000, Liang_Tempered_Real_Values_for_ggplot2[[1]])

png(filename="/home/matteo/Documents/Scienza/Laurea_di_Matematica/Biology_Group_Seminar/picts/MH_simululation_10000_steps.png", width=800,height=800)
	mh_simulation_plot
dev.off()


mh_simulation_plot <- METROPOLIS_HASTINGS_PLOT(1000, Liang_Tempered_Real_Values_for_ggplot2[[1]])
png(filename="/home/matteo/Documents/Scienza/Laurea_di_Matematica/Biology_Group_Seminar/picts/MH_simululation_1000_steps_ex1.png", width=800,height=800)
	mh_simulation_plot
dev.off()

mh_simulation_plot <- METROPOLIS_HASTINGS_PLOT(1000, Liang_Tempered_Real_Values_for_ggplot2[[1]])
png(filename="/home/matteo/Documents/Scienza/Laurea_di_Matematica/Biology_Group_Seminar/picts/MH_simululation_1000_steps_ex2.png", width=800,height=800)
	mh_simulation_plot
dev.off()

	# Parallel Tempering	

pt_simulation_plot <- LIANG_PARALLEL_TEMPERING_PLOT( 1000 )
png(filename="/home/matteo/Documents/Scienza/Laurea_di_Matematica/Biology_Group_Seminar/picts/PT_simululation_1000_steps_1.png", width=800,height=800)
	pt_simulation_plot
dev.off()

pt_simulation_plot <- LIANG_PARALLEL_TEMPERING_PLOT( 1000 )
png(filename="/home/matteo/Documents/Scienza/Laurea_di_Matematica/Biology_Group_Seminar/picts/PT_simululation_1000_steps_2.png", width=800,height=800)
	pt_simulation_plot
dev.off()

pt_simulation_plot <- LIANG_PARALLEL_TEMPERING_PLOT( 10000 )
png(filename="/home/matteo/Documents/Scienza/Laurea_di_Matematica/Biology_Group_Seminar/picts/PT_simululation_10000_steps_1.png", width=800,height=800)
	pt_simulation_plot
dev.off()

png(filename="/home/matteo/Documents/Scienza/Laurea_di_Matematica/Biology_Group_Seminar/picts/PT_simululation_10000_steps_2.png", width=1200,height=1200)
	pt_simulation_plot
dev.off()

	# Parallel Tempering - base temperature.

Parallel <- LIANG_SIMULATION_PARALLEL_TEMPERING( 1000 , FALSE)
Parallel <- LIANG_PARALLEL_TEMPERING_PREPARING_DATA( Parallel, 1000)
LIANG_PARALLEL_TEMPERING_BASE_TEMPERATURE_PLOT(Parallel)


png(filename="/home/matteo/Documents/Scienza/Laurea_di_Matematica/Biology_Group_Seminar/picts/PT_simululation_base_temperature_1000_steps_1.png", width=800,height=800)
	LIANG_PARALLEL_TEMPERING_BASE_TEMPERATURE_PLOT(Parallel)
dev.off()


Parallel <- LIANG_SIMULATION_PARALLEL_TEMPERING( 10000 , FALSE)
Parallel <- LIANG_PARALLEL_TEMPERING_PREPARING_DATA( Parallel, 10000)
png(filename="/home/matteo/Documents/Scienza/Laurea_di_Matematica/Biology_Group_Seminar/picts/PT_simululation_base_temperature_10000_steps_1.png", width=800,height=800)
	LIANG_PARALLEL_TEMPERING_BASE_TEMPERATURE_PLOT(Parallel)
dev.off()

	# STRATEGY_ONE
Parallel <- LIANG_SIMULATION_PARALLEL_TEMPERING( 2000 , STRATEGY_ONE, FALSE)
Parallel <- LIANG_PARALLEL_TEMPERING_PREPARING_DATA( Parallel, 2000)
png(filename="/home/matteo/Documents/Scienza/Laurea_di_Matematica/Biology_Group_Seminar/picts/PT_simululation_base_temperature_2000_steps_strategy_1_try_1.png", width=800,height=800)
	LIANG_PARALLEL_TEMPERING_BASE_TEMPERATURE_PLOT(Parallel)
dev.off()

Parallel <- LIANG_SIMULATION_PARALLEL_TEMPERING( 2000 , STRATEGY_ONE, FALSE)
Parallel <- LIANG_PARALLEL_TEMPERING_PREPARING_DATA( Parallel, 2000)
png(filename="/home/matteo/Documents/Scienza/Laurea_di_Matematica/Biology_Group_Seminar/picts/PT_simululation_base_temperature_2000_steps_strategy_1_try_2.png", width=800,height=800)
	LIANG_PARALLEL_TEMPERING_BASE_TEMPERATURE_PLOT(Parallel)
dev.off()

		# STRATEGY_TWO
Parallel <- LIANG_SIMULATION_PARALLEL_TEMPERING( 2000 , STRATEGY_TWO, FALSE)
Parallel <- LIANG_PARALLEL_TEMPERING_PREPARING_DATA( Parallel, 2000)
png(filename="/home/matteo/Documents/Scienza/Laurea_di_Matematica/Biology_Group_Seminar/picts/PT_simululation_base_temperature_2000_steps_strategy_2_try_1.png", width=800,height=800)
	LIANG_PARALLEL_TEMPERING_BASE_TEMPERATURE_PLOT(Parallel)
dev.off()

Parallel <- LIANG_SIMULATION_PARALLEL_TEMPERING( 2000 , STRATEGY_TWO, FALSE)
Parallel <- LIANG_PARALLEL_TEMPERING_PREPARING_DATA( Parallel, 2000)
png(filename="/home/matteo/Documents/Scienza/Laurea_di_Matematica/Biology_Group_Seminar/picts/PT_simululation_base_temperature_2000_steps_strategy_2_try_2.png", width=800,height=800)
	LIANG_PARALLEL_TEMPERING_BASE_TEMPERATURE_PLOT(Parallel)
dev.off()
	# STRATEGY_THREE
Parallel <- LIANG_SIMULATION_PARALLEL_TEMPERING( 2000 , STRATEGY_THREE, FALSE)
Parallel <- LIANG_PARALLEL_TEMPERING_PREPARING_DATA( Parallel, 2000)
png(filename="/home/matteo/Documents/Scienza/Laurea_di_Matematica/Biology_Group_Seminar/picts/PT_simululation_base_temperature_2000_steps_strategy_3_try_1.png", width=800,height=800)
	LIANG_PARALLEL_TEMPERING_BASE_TEMPERATURE_PLOT(Parallel)
dev.off()


Parallel <- LIANG_SIMULATION_PARALLEL_TEMPERING( 2000 , STRATEGY_THREE, FALSE)
Parallel <- LIANG_PARALLEL_TEMPERING_PREPARING_DATA( Parallel, 2000)
png(filename="/home/matteo/Documents/Scienza/Laurea_di_Matematica/Biology_Group_Seminar/picts/PT_simululation_base_temperature_2000_steps_strategy_3_try_2.png", width=800,height=800)
	LIANG_PARALLEL_TEMPERING_BASE_TEMPERATURE_PLOT(Parallel)
dev.off()

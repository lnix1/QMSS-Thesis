#----------------------------------------------------------------------------------------------#
# The following section brings in the data and produces a set of frames for easier plotting 
# and analysis.
#----------------------------------------------------------------------------------------------#

# This code assumes that you have the 'data_subset' object imported
options(digits=10)

model <- readRDS('Samples/pisa_model_europe_na.rds')

# Extract the draws for all of the parameters
draws <- model$draws()

# Subset for the draws for the person parameters of interest here. Labeled by r_1_1, indicating the first group-level parameter listed in the model, i.e. the person-parameter.
subset_list <- as.data.frame(draws[1,1,])
var_names <- rownames(subset_list)
person_param_names <- var_names[6165:11896]
draws_subset <- draws[,,person_param_names]

# Rescale the draws for the 5732 students to have a mean of 500 and std. of 100.
student_draws_final <- draws_subset*11499.9 + 500.2690058

# Putting together a frame of the student ids with their associate country-language labels
ids <- unique(data_subset$cntstuid)
ids_frame <- as.data.frame(ids)
merged <- unique(merge(ids_frame, data_subset[,c('cntstuid','country_by_language')], by.x='ids', by.y='cntstuid'))
levels(merged$ids) <- levels(ids)
rownames(merged) <- 1:nrow(merged)

# Separating into four country-language datasets for analysis
belgium <- student_draws_final[,,merged$country_by_language=='BELGIUM DUTCH']
united_states <- student_draws_final[,,merged$country_by_language=='UNITED STATES ENGLISH']
netherlands <- student_draws_final[,,merged$country_by_language=='NETHERLANDS DUTCH']
poland <- student_draws_final[,,merged$country_by_language=='POLAND POLISH']

rm('draws', 'draws_subset', 'ids', 'ids_frame', 'merged', 'model', 'person_param_names', 'subset_list', 'var_names', 'data_subset')

#----------------------------------------------------------------------------------------------#



#----------------------------------------------------------------------------------------------#
# The following section is for producing a series of plots to potentially be included in Results.
#----------------------------------------------------------------------------------------------#


# Producing a histogram-density plot for a single student
single_student <- as.data.frame(as.vector(netherlands[,,1]))
names(single_student) <- c('s_1')
plot_single_student <- ggplot(single_student, aes(x=s_1))+
	geom_histogram(bins=85, aes(y = ..density..), fill = 'grey') +
	geom_density() +
	ggtitle('A Single Student (From Netherlands Sample)') +
 	ylab('Estimated Density') +
	xlab('Student Skill in Financial Literacy (With Median Line)')+
	theme_minimal() +
	theme(plot.title = element_text(hjust = 0.5), legend.position = c(1,1))+
	geom_vline(aes(xintercept = median(s_1)),col='blue',size=.5, show.legend=TRUE) +
	scale_y_continuous(expand = c(0,0)) +
	scale_x_continuous(expand = c(0,0))
ggsave("plot_single_student.jpeg", plot=plot_single_student, path="Plots/")


# Producing a graph of student median values for all students in Netherlands with the one student highlighted
netherlands_collapsed <- data.frame()
for (i in 1:4){
  netherlands_collapsed <- rbind(netherlands_collapsed, netherlands[,i,])
}
quant_function <- function(x){
			quantile(x, c(.05, .5, .95))
}
netherlands_percentiles <- as.data.frame(t(apply(netherlands_collapsed, 2, quant_function)))
colnames(netherlands_percentiles) <- c('five', 'fifty', 'nintey_five')
netherlands_percentiles <- netherlands_percentiles[order(netherlands_percentiles[,2]),]
netherlands_percentiles$index <- seq(1, dim(netherlands_collapsed)[2])
netherlands_percentiles$errorColors <- paste('gray', rep(seq(27, 37), dim(netherlands_percentiles)[1]/10)[1:dim(netherlands_percentiles)[1]], sep='')
plot_netherlands_percentiles <- ggplot(netherlands_percentiles, aes(group=index, x=fifty, y=index)) +
					geom_errorbar(aes(xmin=five, xmax=nintey_five, color= errorColors), 
						      alpha=1, width=.1) +
					geom_point() +
					theme_minimal() +
					ggtitle('Medians and 90% Confidence Intervals for All students from The Netherlands') +
 					ylab('Person Index') +
					xlab('Student Skill in Financial Literacy')+
					theme_minimal() +
					theme(plot.title = element_text(hjust = 0.5), legend.position='none') +
					scale_colour_grey() + 
					scale_x_continuous(limits = c(0,1000))
				
plot_netherlands_percentiles
ggsave("plot_netherlands_percentiles.jpeg", plot= plot_netherlands_percentiles, path="Plots/")


# Producing graph of median values and intervals for all four countries
us_values <- as.vector(quantile(as.data.frame(as.vector(united_states))[,1], c(0.05, .25, 0.5, .75, 0.95)))
netherlands_values <- as.vector(quantile(as.data.frame(as.vector(netherlands))[,1], c(0.05, .25, 0.5, .75, 0.95)))
poland_values <- as.vector(quantile(as.data.frame(as.vector(poland))[,1], c(0.05, .25, 0.5, .75, 0.95)))
belgium_values <- as.vector(quantile(as.data.frame(as.vector(belgium))[,1], c(0.05, .25, 0.5, .75, 0.95)))
countries <- c('us', 'netherlands', 'poland', 'belgium')
values_frame <- as.data.frame(rbind(us_values, netherlands_values, poland_values, belgium_values))
values_frame$country <- countries
colnames(values_frame) <- c('five_percent', 'twenty_five', 'median', 'seventy_five', 'ninty_five') 


# Producing all four country densities on a single graph

us_vec <- as.data.frame(as.vector(united_states))
names(us_vec) <- c('s_us')
netherlands_vec <- as.data.frame(as.vector(netherlands))
names(netherlands_vec) <- c('s_neth')
belgium_vec <- as.data.frame(as.vector(belgium))
names(belgium_vec) <- c('s_belgium')
poland_vec <- as.data.frame(as.vector(poland))
names(poland_vec) <- c('s_poland')
collapsed_list <- melt(list('us'=us_vec, 'netherlands'=netherlands_vec, 'poland'=poland_vec, 'belgium'=belgium_vec))


plot_country_densities <- ggplot(collapsed_list, aes(x=value, group=L1, color=L1))+
	geom_density() +
	ggtitle('Density Curves for U.S., Netherlands, Poland, and Belgium') +
 	ylab('Estimated Density') +
	xlab('Skill in Financial Literacy')+
	theme_minimal() +
	theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank())
plot_country_densities
ggsave("plot_country_densities.jpeg", plot= plot_country_densities, path="Plots/")


# Producing graphs or charts of diagnostics
str(model$sampler_diagnostics())
library(posterior)
diagnostics_df <- as_draws_df(model$sampler_diagnostics())
model$cmdstan_diagnose()
model$cmdstan_summary()

#----------------------------------------------------------------------------------------------#

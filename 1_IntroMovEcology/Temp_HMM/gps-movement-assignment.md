# GPS Movement Analysis Assignment

## Background
During yesterday's field exercise, you recorded your movement patterns using GPS tracking. 
This assignment will guide you through analyzing this spatial-temporal data to extract behavioral insights 
and reflect on the relationship between quantitative analysis and lived experience.

## Learning Objectives
- Process and visualize GPS movement data in R
- Work with temporal data and time zone conversions
- Calculate and interpret movement metrics (speed, step length, turn angle)
- Identify behavioral patterns from movement data
- Critically compare quantitative analysis with your actual experience

## Dataset
Your GPS tracking data contains the following information:
- Date and Time (in both GMT and Local time formats)
- Time elapsed in seconds
- Latitude and longitude coordinates
- Horizontal and vertical accuracy measurements
- Altitude data
- Distance, speed, and average speed measurements
- Course and heading information
- Heart rate data (when available)

## Tasks

### Part 1: Data Processing and Visualization
1. Import your GPS data into R
2. Convert timestamps from GMT to local time
3. Create a map visualization of your movement path
4. Calculate the average sampling frequency of your GPS data
5. Create a dot plot showing sampling events through time
6. Regularize the data to a common time step (e.g., 1-minute intervals)

### Part 2: Behavioral Analysis
1. Calculate the following movement metrics:
   - Speed (m/s or km/h)
   - Step length (distance between consecutive points)
   - Turn angle (directional change between consecutive steps)
2. Create color-coded maps of your movement path based on:
   - Speed
   - Step length
   - Turn angle
3. Identify and annotate at least three distinct behavioral states in your movement data
   (e.g., stationary, walking, running, transport)

### Part 3: Reflection (500-750 words)
Reflect on the following questions:
1. How does the quantitative analysis of your movement from GPS data compare to your lived experience moving through these spaces yesterday?
2. What behavioral patterns does the 2D analysis reveal? What aspects of your movement experience are missing from this analysis?
3. How would you characterize the difference between your subjective experience of your movement path and what the GPS data shows?
4. What additional data or contextual information would have enhanced your analysis?
5. How might this type of movement analysis be applied in your field of study?

## Submission Requirements
- R script containing all your code with clear comments
- PDF report including:
  - All required visualizations with captions
  - Clear interpretation of results
  - Reflection section
  - Brief discussion of challenges encountered and how you addressed them

## Due Date
Submit your R script and PDF report via the course portal by [INSERT DATE].

## Grading Criteria
- Correct implementation of data processing and analysis (40%)
- Quality and clarity of visualizations (25%)
- Identification and interpretation of behavioral patterns (15%)
- Depth of reflection comparing quantitative analysis vs. experienced movement (20%)

/* timetest.h */

#define NUM_TIMERS 25
#define TIMER_INTERVAL 1000


/*
 * We'll use one callback for all of the timers, and use
 * the client data to tell which is which.
 *
 * The client data will be a pointer to the following structure:
 */
typedef struct
{
  UInt32_t      Interval;
  int           NumberOfTimesSent;
  Boolean_t     Continue;
} TimerInfo_s;


/* All of the global variables are collected in one structure */
typedef struct
{
  TimerInfo_s TimerArray[NUM_TIMERS];
  int         SelectedTimer;
} TimerTestState_s;



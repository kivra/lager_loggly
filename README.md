Overview
============

This is a Loggly backend for lager which lets you send lager logs to your Loggly account.

##Configuration
Configure a Lager handler like the following:

	{lager_loggly_backend, [Identity, Level, MaxRetries, RetryInterval, LogglyUrl]}
	
* Identity - The string that all messages get tagged with in Loggly
* Level - The lager level at which the  backend accepts messages (eg. using ‘info’ would send all messages at info level or above into syslog)
* MaxRetries - The maximum number of retries the backend will do before giving up on Loggly
* RetryInterval - The interval at which each retry is performed. i.e. Retries 5 and Interval 3 means that it will try a maximum of 5 times with 3 seconds apart
* LogglyUrl - This is your unique Loggly URL for a given Input


An example might look something like this:

	{lager_loggly_backend, [<<"my_id">>, info, 5, 3, "https://logs.loggly.com/inputs/1c6b53a4-972b-4c69-83ea-037de24c9bb2"]}
Refer to Lager’s documentation for futher information on configuring handlers.
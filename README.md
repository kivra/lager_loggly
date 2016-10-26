[![Hex.pm](https://img.shields.io/hexpm/v/lager_loggly.svg?maxAge=2592000)](https://hex.pm/packages/lager_loggly) Overview
============

This is a Loggly backend for lager which lets you send lager logs to your Loggly account.

##Configuration
Configure a Lager handler like the following:

	{lager_loggly_backend, [Level, MaxRetries, RetryInterval, LogglyUrl]}

* Level - The lager level at which the  backend accepts messages (eg. using ‘info’ would send all messages at info level or above into syslog)
* MaxRetries - The maximum number of retries the backend will do before giving up on Loggly
* RetryInterval - The interval at which each retry is performed. i.e. Retries 5 and Interval 3 means that it will try a maximum of 5 times with 3 seconds apart
* LogglyUrl - This is your unique Loggly URL for a given Input, including your application specific identity tag


An example might look something like this:

	{lager_loggly_backend, [info, 5, 3, "https://logs-01.loggly.com/inputs/1c6b53a4-972b-4c69-83ea-037de24c9bb2/tag/my_id/"]}

Refer to Lager’s documentation for further information on configuring handlers.

##Upgrade notes

Since the identity tag should be appended directly to the `LogglyUrl` while accessing the current Loggly API, the previous configuration option `Identity` was removed.

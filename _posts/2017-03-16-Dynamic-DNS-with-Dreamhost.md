Most home ISP's assign you a dynamic IP address that changes periodically. So, what do you do if you want to run a server at home and access it remotely, but don't know your IP address? If you already own a domain name and an account with a hosting provider that provides an API and DNS services, you can create your own dynamic DNS solution that you can automate on a Raspberry Pi. This article will show you how with a [Dreamhost](https://www.dreamhost.com/r.cgi?201755) account and Python. (Disclaimer: Dreamhost is the provider I use, and the link is a referral link.)

[Dreamhost](https://www.dreamhost.com/r.cgi?201755) provides an [API](https://api.dreamhost.com) that can create, remove, and edit DNS records. Our application will use the API to update the A record of a domain with your home's IP address. Let's take a look at the API's [DNS functions](https://help.dreamhost.com/hc/en-us/articles/217555707-DNS-API-commands):

- ``dns-list_records`` returns a list of all DNS records in your account.
- ``dns-add_record`` adds a DNS record.
- ``dns-remove_record`` removes a DNS record.

We will be using these API functions to update the A record of a domain with your home's IP address. 

Before we get started, you'll need a domain name with its DNS hosted by Dreamhost. You will also need an API key from Dreamhost with permission to use the DNS API functions.

**The Code**

Now, let's dive into the code. First, import the modules we'll need and define our API key:

```python
import requests
import uuid
import os
import os.path
import sys
import logging
import time

API_KEY = 'put_your_api_key_here'
DREAMHOST_URL = 'https://api.dreamhost.com'
IP_URL = 'https://ipinfo.io'
```

For maintainability and reusability, we'll divide our code into functions. One of the required tasks is finding out our current IP address. An easy way to do this is to get our IP address from a web service using the `requests` library. We'll be using [IPInfo](https://ipinfo.io), which returns a JSON document containing your IP address.

```python
def get_ip():
    response = requests.get(IP_URL)
    ip = response.json()['ip']
    return ip
```

We'll also need a function to send commands to the Dreamhost API.

```python
def send_dreamhost_command(cmd, **kwargs):
    """
    Send a request to the Dreamhost API and return the response in JSON.
    """
    params = "?key={}&cmd={}&unique_id={}&format=json".format(API_KEY, cmd, uuid.uuid1())
    for key, value in kwargs.items():
        params += "&{}={}".format(key, value)
    response = requests.get(DREAMHOST_URL + '/' + params)
    return response.json()
```

Each API requests requires a unique id. `uuid.uuid1()` generates  this ID.

To obtain the IP stored in the current DNS A record, we'll need to call `send_dreamhost_command` with a `dns-list_records` command sent to the API. `dns-list_records` returns a JSON document that looks like:

```json
{ 'data':
  [{'comment': '', 'account_id': 'your_account_id', 'zone': 'yourdomain.com', 'editable': '0', 'value': '10.1.1.1', 'record': 'yourdomain.com', 'type': 'A' }]
}
```

Each DNS record will be a JSON subdocument in the data array. We will find the record in this array that lists the IP for the domain we're using for dynamic DNS.


```python
def get_dns_ip():
    """
    Retrieves the current IP of DYNAMIC_URL from DNS.
    """
    dns = send_dreamhost_command('dns-list_records')
    ip = ''
    for dns_record in dns['data']:
        if dns_record['record'] == config['DYNAMIC_URL'] and dns_record['type'] == 'A':
            ip = dns_record['value']
            logging.info('Found record with IP ' + ip)
    return ip
```

If the IP address returned by `get_dns_ip()` does not match the IP address returned by `get_ip()`, we'll also to update the DNS record by removing the old record and adding a new A record. The function `update_ip(old_ip, new_ip)` does this by sending the API commands `dns-remove_record` followed by `dns-add_record`.

```python
def update_ip(old_ip, new_ip):
    """
    Sets the A record of DYNAMIC_URL to the current IP.
    """
    resp = send_dreamhost_command('dns-remove_record', record=config['DYNAMIC_URL'], type='A', value=old_ip)
    if resp['result'] == 'error':
        logging.error('Failed to remove old record. Error: ' + resp['data'])
    resp = send_dreamhost_command('dns-add_record', record=config['DYNAMIC_URL'], type='A', value=new_ip)
    if resp['result'] == 'error':
        logging.error('Failed to add record for ' + config['DYNAMIC_URL'] + ' with IP ' + new_ip)
        print('Error:' + resp['data'])
    else:
        logging.info('Changed IP for ' + config['DYNAMIC_URL'] + ' from ' + old_ip + ' to ' + new_ip)
```

Finally, let's write the logic that will connect these functions:

```python
def main():
    ip = get_ip()
    old_ip = get_dns_ip()
    if ip == old_ip:
        logging.info("IP address has not changed.")
    else:
        update_ip(old_ip, ip)
    logging.info('** Ending {} on {} **'.format(os.path.basename(sys.argv[0]), time.asctime()))

if __name__ == "__main__":
    main()
```

We call `get_ip()` to get our current home IP, and we call `get_dns_ip()` to get the IP in our DNS record. We compare the two IP addresses. If they are not the same, we call `update_ip()` to update the DNS record. Now, all that's left is to put it in a cron job on a Raspberry Pi or any other server you run at home, and you'll be running your own dynamic DNS service.

My full code, which uses configuration files to store global constants, is available on [Github](https://github.com/shang-lin/dyndream). 




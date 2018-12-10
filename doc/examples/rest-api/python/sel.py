import requests
import json
import pprint

# beginning of the URLs for all the SEL APIs
url_base = 'http://127.0.0.1:9003/'

# convert a relative url to an absolute one
def url_(path):
    return url_base + path

def str_result(resp):
    return resp._content.decode('ascii')

def oid_result(resp):
    return int(resp._content)

# track the most recently created client_id
client_id = None

#
# client/session management
#

# Create a new client, returns the new client id.
# Sets global client_id as side effect.
#
def create_client():
    global client_id
    client_id = str_result(requests.post(url_('client'),
                         json = {'max-population-size': 1024},
                         headers = {'Content-Type': 'application/json'}))
    return client_id

#
# software object management
#

# Create a new software object, of the requested type, using
# the passed dictionary of initializers.
# Returns the software oid (object id)
#
def create_software(software_type, initializers):
    global client_id
    return oid_result(requests.post(url_('soft?cid={}&type={}'.format(client_id, software_type)),
                         json=initializers,
                         headers = { 'Content-Type': 'application/json'}))

# get all software objects associated with the current client
# Returns a list of software oids.
#
def get_all_software():
    global client_id
    return requests.get(url_('soft?cid={}'.format(client_id)),
                         headers = { 'Accept': 'application/json'}).json()

# Get details of a specific software object, given its oid.
#
def get_software(oid):
    global client_id
    return requests.get(url_('soft?cid={}&sid={}'.format(client_id, oid)),
                         headers = { 'Accept': 'application/json'}).json()

#
# Software population management
#

# Create a new software population object.
# The unique name for the population (string) may be provided.
# If name is unspecified (None), then a unique name will be generated.
# The type of software (software_type) and initial software oids may
# be provided in the initializer dict.
#
def create_population(name, initializers):
    global client_id
    return str_result(requests.post(url_('population?cid={}{}'.format(
        client_id,
        '&"name": "{}"'.format(name) if name else '')),
                         json=initializers,
                         headers = { 'Content-Type': 'application/json'}))
 
# Get all population objects associated with the current client
# Returns a list of population ids (unique names).
#
def get_all_populations():
    global client_id
    return requests.get(url_('population?cid={}'.format(client_id)),
                         headers = { 'Accept': 'application/json'}).json()

# Get details of a specific population, given its name.
#
def get_population(name):
    global client_id
    return requests.get(url_('population?cid={}&name="{}"'.format(client_id, name)),
                         headers = { 'Accept': 'application/json'}).json()

# Add software objects to a specific population, given its name.
# sids: an array of software ids to add
#
def update_population(name, sids):
    global client_id
    return requests.put(url_('population?cid={}&name="{}"'.format(client_id, name)),
                        json = { "sids": sids },
                        headers = { 'Accept': 'application/json'}).json()

#
# Mutation management
#

# Create a new mutation object, of the requested type, given a software object,
# mutation type and targets.
# Returns the mutation oid (object id).
#
def create_mutation(mutation_type, oid, targets):
    global client_id
    return oid_result(requests.post(url_('mut?cid={}'.format(client_id)),
                             json = { 'type': mutation_type, 'sid': oid, 'targets': targets },
                         headers = { 'Content-Type': 'application/json'}))

# Get all mutation objects associated with the current client.
# Returns a list of mutation oids.
#
def get_all_mutations():
    global client_id
    return requests.get(url_('mut?cid={}'.format(client_id)),
                         headers = { 'Accept': 'application/json'}).json()

# Get details of a specific mutation, given its oid.
#
def get_mutation(oid):
    global client_id
    return requests.get(url_('mut?cid={}&mid={}'.format(client_id, oid)),
                         headers = { 'Accept': 'application/json'}).json()

#
# Async Job/Task management
#

# Create a new async_job, of the requested type, given a job name,
# population id, func name and num_threads.
# Returns the async job name.
#
def create_async_job(job_name, population, func, num_threads):
    global client_id
    return str_result(requests.post(url_('async?cid={}&name="{}"'.format(client_id, job_name)),
                             json = { 'pid': population, 'func': func,
                                      'threads': num_threads },
                         headers = { 'Content-Type': 'application/json'}))

# Get all async_jobs associated with the current client.
# Returns a list of job names.
#
def get_all_async_jobs():
    global client_id
    return requests.get(url_('async?cid={}'.format(client_id)),
                         headers = { 'Accept': 'application/json'}).json()

# Get details of a specific async job, given its name.
def get_async_job(name):
    global client_id
    return requests.get(url_('async?cid={}&name="{}"'.format(client_id, name)),
                         headers = { 'Accept': 'application/json'}).json()

#
# Test Suite management
#

# Create a new test suite, containing the passed program-name/program-args pairs.
# Returns the test-suite oid (object id).
#
def create_tests(tests):
    global client_id
    return oid_result(requests.post(url_('tests?cid={}'.format(client_id)),
                             json = tests,
                         headers = { 'Content-Type': 'application/json'}))

# Get all mutation objects associated with the current client.
# Returns a list of mutation oids.
#
def get_all_tests():
    global client_id
    return requests.get(url_('tests?cid={}'.format(client_id)),
                         headers = { 'Accept': 'application/json'}).json()

# Get details of a specific mutation, given its oid.
#
def get_tests(oid):
    global client_id
    return requests.get(url_('tests?cid={}&oid={}'.format(client_id, oid)),
                         headers = { 'Accept': 'application/json'}).json()

#
# Instrumented software management
#

# Create a new software object, which is a copy of the argument software,
# with added instrumentation.
# Returns the new software oid.
#
def create_instrumented(software_oid):
    global client_id
    return oid_result(requests.post(
        url_('instrumented?cid={}&sid={}'.format(client_id, software_oid)),
                         headers = { 'Content-Type': 'application/json'}))

# Get all instrumented software objects associated with the current client
# Returns a list of software oids.
#
def get_all_instrumented():
    global client_id
    return requests.get(url_('instrumented?cid={}'.format(client_id)),
                         headers = { 'Accept': 'application/json'}).json()

# Get details of a specific instrumented software object, given its oid.
#
def get_instrumented(oid):
    global client_id
    return requests.get(url_('instrumented?cid={}&sid={}'.format(client_id, oid)),
                         headers = { 'Accept': 'application/json'}).json()

#
# Traced software management
#

# Trace an instrumented software object, running the selected tests.
# inst_bin: path of the software executable to trace
# The trace results will be stored in the trace-db, within the software object.
# Returns the software oid (same one that was passed in).
#
def create_trace(software_oid, tests_oid, inst_bin):
    global client_id
    return oid_result(requests.post(url_('tracesoft?cid={}&sid={}&tests-oid={}'.format(
        client_id, software_oid, tests_oid)),
                             json = {"inst-bin": inst_bin},
                             headers = { 'Content-Type': 'application/json'}))

#
# Scion management
#

# Create a scion object, of the requested type.
# Returns the scion oid (object id).
#
def create_scion(scion_type):
    global client_id
    return oid_result(requests.post(url_('scionref?cid={}'.format(client_id)),
                             json = { 'name': scion_type },
                         headers = { 'Content-Type': 'application/json'}))

# Get all scion objects associated with the current client.
# Returns a list of scion oids.
#
def get_all_scions():
    global client_id
    return requests.get(url_('scionref?cid={}'.format(client_id)),
                         headers = { 'Accept': 'application/json'}).json()

# Get details of a specific scion, given its oid.
#
def get_scion(oid):
    global client_id
    return requests.get(url_('scionref?cid={}&oid={}'.format(client_id, oid)),
                         headers = { 'Accept': 'application/json'}).json()

#
# Trace Results management
#

# Create a trace results object, of the specified name,
# collected from the trace-db of the designated software
# object (which should have been previously instrumented and traced)
# applied to the specified scion.
# Returns the trace results name.
#
def create_trace_results(name, software_oid, scion_oid):
    global client_id
    return str_result(requests.post(url_('traceres?cid={}&name="{}"'.format(client_id, name)),
                             json = { 'sid': software_oid, 'scion-oid': scion_oid },
                         headers = { 'Content-Type': 'application/json'}))

# Get all trace results objects associated with the current client
# Returns a list of trace results identifiers.
#
def get_all_trace_results():
    global client_id
    return requests.get(url_('traceres?cid={}'.format(client_id)),
                         headers = { 'Accept': 'application/json'}).json()

# Get details of a specific trace results, given its name.
#
def get_trace_results(name):
    global client_id
    return requests.get(url_('traceres?cid={}&name="{}"'.format(client_id, name)),
                         headers = { 'Accept': 'application/json'}).json()

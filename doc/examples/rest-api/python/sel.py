""" SEL REST API interface wrapper for Python. """

import requests

# beginning of the URLs for all the SEL APIs
URL_BASE = 'http://127.0.0.1:9003/'

def url_(path):
    """ Convert a relative url to an absolute one. """
    return URL_BASE + path

def str_result(resp):
    """ Convert a response to an ASCII string. """
    # FIXME use `r.text` or `r.content` here.
    return resp._content.decode('ascii')

def oid_result(resp):
    """ Convert a response's content to an integer. """
    # FIXME use `r.text` or `r.content` here.
    return int(resp._content)

# track the most recently created CLIENT_ID
CLIENT_ID = None

#### client/session management

def create_client():
    """ Create a new client, returns the new client id.
        Sets global CLIENT_ID as side effect.
    """
    global CLIENT_ID
    CLIENT_ID = str_result(requests.post(url_('client'),
                                         json={'max-population-size': 1024},
                                         headers={'Content-Type': 'application/json'}))
    return CLIENT_ID

#### software object management

def create_software(software_type, initializers):
    """ Create a new software object, of the requested type, using the passed
        dictionary of initializers.
        Returns the software oid (object id).
    """
    global CLIENT_ID
    return oid_result(requests.post(url_('soft?cid={}&type={}'.format(CLIENT_ID, software_type)),
                                    json=initializers,
                                    headers={'Content-Type': 'application/json'}))

def get_all_software():
    """ Get all software objects associated with the current client.
        Returns a list of software oids.
    """
    global CLIENT_ID
    return requests.get(url_('soft?cid={}'.format(CLIENT_ID)),
                        headers={'Accept': 'application/json'}).json()

def get_software(oid):
    """ Get details of a specific software object, given its oid. """
    global CLIENT_ID
    return requests.get(url_('soft?cid={}&sid={}'.format(CLIENT_ID, oid)),
                        headers={'Accept': 'application/json'}).json()

#### Software population management

#TODO Consider using a default value for `name`.
def create_population(name, initializers):
    """ Create a new software population object.
        The unique name for the population (string) may be provided.If name is
        unspecified (None), then a unique name will be generated. The type of
        software (software_type) and initial software oids may be provided in
        the initializer dict.
    """
    global CLIENT_ID
    return str_result(requests.post(url_('population?cid={}{}'.format(
        CLIENT_ID,
        '&"name": "{}"'.format(name) if name else '')),
                                    json=initializers,
                                    headers={'Content-Type': 'application/json'}))

def get_all_populations():
    """ Get all population objects associated with the current client
        Returns a list of population ids (unique names).
    """
    global CLIENT_ID
    return requests.get(url_('population?cid={}'.format(CLIENT_ID)),
                        headers={'Accept': 'application/json'}).json()

def get_population(name):
    """ Get details of a specific population, given its name. """
    global CLIENT_ID
    return requests.get(url_('population?cid={}&name="{}"'.format(CLIENT_ID, name)),
                        headers={'Accept': 'application/json'}).json()

#
def update_population(name, sids):
    """ Add software objects to a specific population, given its name.

        Args:
        - sids: an array of software ids to add
    """
    global CLIENT_ID
    return requests.put(url_('population?cid={}&name="{}"'.format(CLIENT_ID, name)),
                        json={"sids": sids},
                        headers={'Accept': 'application/json'}).json()

#### Mutation management

def create_mutation(mutation_type, oid, targets, scion):
    """ Create a new mutation object, of the requested type, given a software
        object, mutation type and targets.
        Returns the mutation oid (object id).
    """
    global CLIENT_ID
    return oid_result(requests.post(url_('mut?cid={}'.format(CLIENT_ID)),
                                    json={'type': mutation_type,
                                          'sid': oid,
                                          'targets': targets,
                                          'scion': "SCION:"+ str(scion)},
                                    headers={'Content-Type': 'application/json'}))

def get_all_mutations():
    """ Get all mutation objects associated with the current client.
        Returns a list of mutation oids.
    """
    global CLIENT_ID
    return requests.get(url_('mut?cid={}'.format(CLIENT_ID)),
                        headers={'Accept': 'application/json'}).json()

def get_mutation(oid):
    """ Get details of a specific mutation, given its oid. """
    global CLIENT_ID
    return requests.get(url_('mut?cid={}&mid={}'.format(CLIENT_ID, oid)),
                        headers={'Accept': 'application/json'}).json()

#### Async Job/Task management

def create_async_job(job_name, pid, population, func, num_threads):
    """ Create a new async_job, of the requested type, taking, as arguments:

        Args:
        - job name (str)
        - population id (int)
        - population (list)
        - func name (str)
        - num_threads (int)

        Returns the async job name.

        If a software population object is being used, supply the appropriate pid and
        pass None for the population argument. If we are passing a list of custom
        lisp objects, pass None for pid, and a list of lisp objects for population.
        Each item in population will spawn an asynchronous task, being passed to func.
    """
    global CLIENT_ID
    return str_result(requests.post(url_('async?cid={}&name="{}"'.format(CLIENT_ID, job_name)),
                                    json={'pid': pid, 'population' : population, 'func': func,
                                          'threads': num_threads},
                                    headers={'Content-Type': 'application/json'}))

def get_all_async_jobs():
    """ Get all async_jobs associated with the current client.
        Returns a list of job names.
    """
    global CLIENT_ID
    return requests.get(url_('async?cid={}'.format(CLIENT_ID)),
                        headers={'Accept': 'application/json'}).json()

def get_async_job(name):
    """ Get details of a specific async job, given its name. """
    global CLIENT_ID
    return requests.get(url_('async?cid={}&name="{}"'.format(CLIENT_ID, name)),
                        headers={'Accept': 'application/json'}).json()

#### Test Suite management

def create_tests(tests):
    """ Create a new test suite, containing the passed program-name/program-args
        pairs.
        Returns the test-suite oid (object id).
    """
    global CLIENT_ID
    return oid_result(requests.post(url_('tests?cid={}'.format(CLIENT_ID)),
                                    json=tests,
                                    headers={'Content-Type': 'application/json'}))

def get_all_tests():
    """ Get all mutation objects associated with the current client.
        Returns a list of mutation oids.
    """
    global CLIENT_ID
    return requests.get(url_('tests?cid={}'.format(CLIENT_ID)),
                        headers={'Accept': 'application/json'}).json()

def get_tests(oid):
    """ Get details of a specific mutation, given its oid. """
    global CLIENT_ID
    return requests.get(url_('tests?cid={}&oid={}'.format(CLIENT_ID, oid)),
                        headers={'Accept': 'application/json'}).json()


#### Instrumented software management

def create_instrumented(software_oid):
    """ Create a new software object, which is a copy of the argument software,
        with added instrumentation.
        Returns the new software oid.
    """
    global CLIENT_ID
    return oid_result(requests.post(
        url_('instrumented?cid={}&sid={}'.format(CLIENT_ID, software_oid)),
        headers={'Content-Type': 'application/json'}))


def get_all_instrumented():
    """ Get all instrumented software objects associated with the current client
        Returns a list of software oids.
    """
    global CLIENT_ID
    return requests.get(url_('instrumented?cid={}'.format(CLIENT_ID)),
                        headers={'Accept': 'application/json'}).json()

def get_instrumented(oid):
    """ Get details of a specific instrumented software object, given its oid. """
    global CLIENT_ID
    return requests.get(url_('instrumented?cid={}&sid={}'.format(CLIENT_ID, oid)),
                        headers={'Accept': 'application/json'}).json()

#### Traced software management

def create_trace(software_oid, tests_oid, inst_bin):
    """ Trace an instrumented software object, running the selected tests.

        Args:
        - inst_bin: path of the software executable to trace

        The trace results will be stored in the trace-db, within the software object.
        Returns the software oid (same one that was passed in).
    """
    global CLIENT_ID
    return oid_result(requests.post(url_('tracesoft?cid={}&sid={}&tests-oid={}'.format(
        CLIENT_ID, software_oid, tests_oid)),
                                    json={"inst-bin": inst_bin},
                                    headers={'Content-Type': 'application/json'}))

#### Scion management

def create_scion(scion_type):
    """ Create a scion object, of the requested type.
        Returns the scion oid (object id).
    """
    global CLIENT_ID
    return oid_result(requests.post(url_('scionref?cid={}'.format(CLIENT_ID)),
                                    json={'name': scion_type},
                                    headers={'Content-Type': 'application/json'}))


def get_all_scions():
    """ Get all scion objects associated with the current client.
        Returns a list of scion oids.
    """
    global CLIENT_ID
    return requests.get(url_('scionref?cid={}'.format(CLIENT_ID)),
                        headers={'Accept': 'application/json'}).json()

def get_scion(oid):
    """ Get details of a specific scion, given its oid. """
    global CLIENT_ID
    return requests.get(url_('scionref?cid={}&oid={}'.format(CLIENT_ID, oid)),
                        headers={'Accept': 'application/json'}).json()

#### Trace Results management

def create_trace_results(name, software_oid, orig_software_oid, scion_oid):
    """" Create a trace results object, of the specified name, collected from
         the trace-db of the designated software object (which should have been
         previously instrumented and traced) applied to the specified scion.
         Returns the trace results name.
    """
    global CLIENT_ID
    return str_result(requests.post(url_('traceres?cid={}&name="{}"'.format(CLIENT_ID, name)),
                                    json={'sid': software_oid,
                                          'osid': orig_software_oid,
                                          'scion-oid': scion_oid},
                                    headers={'Content-Type': 'application/json'}))

def get_all_trace_results():
    """ Get all trace results objects associated with the current client.
        Returns a list of trace results identifiers.
    """
    global CLIENT_ID
    return requests.get(url_('traceres?cid={}'.format(CLIENT_ID)),
                        headers={'Accept': 'application/json'}).json()

def get_trace_results(name):
    """ Get details of a specific trace results, given its name. """
    global CLIENT_ID
    return requests.get(url_('traceres?cid={}&name="{}"'.format(CLIENT_ID, name)),
                        headers={'Accept': 'application/json'}).json()


#### Mutated/Injected software management

def create_injections(software_oid, mutation_oid):
    """ Create a new software object, which is a copy of the argument software,
        with mutations. Returns the new software oids.
    """
    global CLIENT_ID
    return requests.post(
        url_('inject?cid={}&sid={}&mid={}'.format(
            CLIENT_ID, software_oid, mutation_oid)),
        headers={'Content-Type': 'application/json'}).json()

def write_software(software_oid, path):
    """ Write software to file. """
    global CLIENT_ID
    return requests.post(url_('writesoft?cid={}&sid={}'.format(CLIENT_ID,
                                                               software_oid)),
                         json={"path": path},
                         headers={'Content-Type': 'application/json'})

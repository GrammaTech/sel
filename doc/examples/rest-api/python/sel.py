""" SEL REST API interface wrapper for Python. """

import requests

#### Helper functions

def str_result(resp):
    """ Convert a response to an ASCII string. """
    return resp.text

def oid_result(resp):
    """ Convert a response's content to an integer. """
    return int(resp.text)

#### Main class

class SelRest:
    """ This class represents a session with an SEL REST server.
        Upon creation, it registers as a new client with the server.
    """
    def __init__(self, _urlbase="http://127.0.0.1:9004/"):
        self._urlbase = _urlbase
        self._client_id = str_result(requests.post(self._url('client'),
                                                   json={'max-population-size' : 1024},
                                                   headers={'Content-Type' : 'application/json'}))

    def _url(self, path):
        """ Helper to convert a relative url to an absolute one. """
        return ''.join([self._urlbase, path])

    #### Software object management

    def create_software(self, software_type, initializers):
        """ Create a new software object, of the requested type, using the passed
            dictionary of initializers.
            Returns the software oid (object id).
        """
        return oid_result(requests.post(self._url('soft'),
                                        params={'cid' : self._client_id, 'type' : software_type},
                                        json=initializers,
                                        headers={'Content-Type': 'application/json'}))

    def get_all_software(self):
        """ Get all software objects associated with the current client.
            Returns a list of software oids.
        """
        return requests.get(self._url('soft'), params={'cid' : self._client_id},
                            headers={'Accept': 'application/json'}).json()

    def get_software(self, oid):
        """ Get details of a specific software object, given its oid. """
        return requests.get(self._url('soft'),
                            params={'cid' : self._client_id, 'sid' : oid},
                            headers={'Accept': 'application/json'}).json()

    #### Software population management

    def create_population(self, initializers, name=None):
        """ Create a new software population object.
            The unique name for the population (string) may be provided. If name is
            unspecified (None), then a unique name will be generated. The type of
            software (software_type) and initial software oids may be provided in
            the initializer dict.
        """
        params = {'name' : str(name)} if name else {}
        params['cid'] = self._client_id
        return str_result(requests.post(self._url('population'),
                                        params=params,
                                        json=initializers,
                                        headers={'Content-Type': 'application/json'}))

    def get_all_populations(self):
        """ Get all population objects associated with the current client
            Returns a list of population ids (unique names).
        """
        return requests.get(self._url('population'), params={'cid' : self._client_id},
                            headers={'Accept': 'application/json'}).json()

    def get_population(self, name):
        """ Get details of a specific population, given its name. """
        return requests.get(self._url('population'),
                            params={'cid' : self._client_id, 'name' : name},
                            headers={'Accept': 'application/json'}).json()

    #
    def update_population(self, name, sids):
        """ Add software objects to a specific population, given its name.

            Args:
            - sids: an array of software ids to add
        """
        return requests.put(self._url('population'),
                            params={'cid' : self._client_id, 'name' : name},
                            json={"sids": sids},
                            headers={'Accept': 'application/json'}).json()

    #### Mutation management

    def create_mutation(self, mutation_type, oid, targets, scion):
        """ Create a new mutation object, of the requested type, given a software
            object, mutation type and targets.
            Returns the mutation oid (object id).
        """
        return oid_result(requests.post(self._url('mut'), params={'cid' : self._client_id},
                                        json={'type': mutation_type,
                                              'sid': oid,
                                              'targets': targets,
                                              'scion': "SCION:"+ str(scion)},
                                        headers={'Content-Type': 'application/json'}))

    def get_all_mutations(self):
        """ Get all mutation objects associated with the current client.
            Returns a list of mutation oids.
        """
        return requests.get(self._url('mut'), params={'cid' : self._client_id},
                            headers={'Accept': 'application/json'}).json()

    def get_mutation(self, oid):
        """ Get details of a specific mutation, given its oid. """
        return requests.get(self._url('mut'), params={'cid' : self._client_id, 'mid' : oid},
                            headers={'Accept': 'application/json'}).json()

    #### Async Job/Task management

    def create_endpoint_job(self, endpoint, values):
        """ Create a new endpoint job.

            Args:
            - endpoint (string) : The endpoint to use.
            - values (dict) : Dictionary of values the endpoint expects

            Returns the job name.
        """
        return str_result(requests.post(self._url(endpoint),
                                        params={'cid' : self._client_id},
                                        json=values,
                                        headers={'Content-Type': 'application/json'}))

    def get_all_endpoint_jobs(self, endpoint):
        """ Get all jobs associated with the current client and endpoint.

            Args:
            - endpoint (string) : The endpoint to use.

            Returns a list of job names.
        """
        return requests.get(self._url(endpoint), params={'cid' : self._client_id},
                            headers={'Accept': 'application/json'}).json()

    def get_endpoint_job(self, endpoint, name):
        """ Get details of a specific endpoint job, given the endpoint and job name. """
        return requests.get(self._url(endpoint),
                            params={'cid' : self._client_id, 'name' : name},
                            headers={'Accept': 'application/json'}).json()

    #### Async Job/Task management

    def create_async_population_job(self, job_name, pid, func, num_threads):
        """ Create a new async_job, of the requested type, taking, as arguments:

            Args:
            - job name (str)
            - population id (int)
            - func name (str)
            - num_threads (int)

            Returns the async job name.

            If a software population object is being used, supply the appropriate pid.
            If we are passing a list of custom lisp objects, use create_async_job instead.
            We will spawn an asynchronous task for each population entity,
            applying `func` to it.
        """
        return str_result(requests.post(self._url('async'),
                                        params={'cid' : self._client_id, 'name' : job_name},
                                        json={'pid' : pid,
                                              'func' : func,
                                              'threads' : num_threads},
                                        headers={'Content-Type': 'application/json'}))

    def create_async_job(self, job_name, arguments, func, num_threads):
        """ Create a new async_job, of the requested type, taking, as input:

            Args:
            - job name (str)
            - population id (int)
            - arguments (list)
            - func name (str)
            - num_threads (int)

            Returns the async job name.

            Arguments is a list of lisp objects to apply the function to. If you
            would prefer to use a population of software objects, use
            `create_async_population_job` instead.
            A new asynchronous task is started, calling the function on the arguments.
        """
        return str_result(requests.post(self._url('async'),
                                        params={'cid' : self._client_id, 'name' : job_name},
                                        json={'arguments' : [arguments],
                                              'func' : func,
                                              'threads' : num_threads},
                                        headers={'Content-Type': 'application/json'}))

    def get_all_async_jobs(self):
        """ Get all async_jobs associated with the current client.
            Returns a list of job names.
        """
        return requests.get(self._url('async'), params={'cid' : self._client_id},
                            headers={'Accept': 'application/json'}).json()

    def get_async_job(self, name):
        """ Get details of a specific async job, given its name. """
        return requests.get(self._url('async'),
                            params={'cid' : self._client_id, 'name' : name},
                            headers={'Accept': 'application/json'}).json()

    #### Test Suite management

    def create_tests(self, tests):
        """ Create a new test suite, containing the passed program-name/program-args
            pairs.
            Returns the test-suite oid (object id).
        """
        return oid_result(requests.post(self._url('tests'),
                                        params={'cid' : self._client_id},
                                        json=tests,
                                        headers={'Content-Type': 'application/json'}))

    def get_all_tests(self):
        """ Get all mutation objects associated with the current client.
            Returns a list of mutation oids.
        """
        return requests.get(self._url('tests'), params={'cid' : self._client_id},
                            headers={'Accept': 'application/json'}).json()

    def get_tests(self, oid):
        """ Get details of a specific mutation, given its oid. """
        return requests.get(self._url('tests'),
                            params={'cid' : self._client_id, 'oid' : oid},
                            headers={'Accept': 'application/json'}).json()

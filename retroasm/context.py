from .linereader import BadInput

class NameExistsError(BadInput):
    '''Raised when attempting to add an element to a context under a name
    which is already in use.
    '''

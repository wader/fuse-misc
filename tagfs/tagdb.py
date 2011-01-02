# TODO: make them inherit?
class TagDBError(Exception): pass
class TagDBTagAlreadyExists(Exception): pass
class TagDBFileAlreadyExists(Exception): pass
class TagDBTagHasFiles(Exception): pass
class TagDBTagDoesNotExist(Exception): pass
class TagDBFileDoesNotExist(Exception): pass



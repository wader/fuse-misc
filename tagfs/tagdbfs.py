#
# Tag database using another filesystem as storage
#
# mattias.wadman@galaxen.se
#
# tags/tag1/1
# tags/tag1/2
# files/1
#   content:
#   path\n
#   tag1\n
#   tag2
# files/2
# map/file1 -> 1
# map/file2 -> 2
#
# tags: one directory for each tag containing empty files named the id of the
#       files beloging to the tag
# files: files named by their id contains full path to file and list of
#        tags file belongs to
# map: used to map basename to file ids (also makes sure basename stayes
#      unique)
# 
# 
# Known issues:
# Currently paths and tags can not contain new line characters
# dircache, fast but probably uses some memory
# dircache.reset() ?
#
# Ideas:
# If one file for each id belonging to a tag becomes to slow, invent some
# kind of hashing to group ids to bigger files
# Could be used for files/* too
# Even into one file? add/remove will be slow but maybe not problem?
#


import os
import errno
import dircache

from tagdb import *


# like os.makedirs but skips if path exists
def _makedirs(path):
    if os.path.exists(path):
        return
    os.makedirs(path)



class TagDBFS:
    def __init__(self, *argv, **kw):
        self.tagdb = kw["tagdb"]
        self.id_seq = 0

        # if fsck, skip init
        if kw.has_key("fsck"):
            return

        _makedirs(self.make_path_tags())
        _makedirs(self.make_path_files())
        _makedirs(self.make_path_map())

        readme = self.make_path("README")
        if not os.path.exists(readme):
            f = open(readme, "w")
            f.write("""\
tagfs data tagdb, read comment in tagdbfs.py for details
""")
            f.close()

        for n in dircache.listdir(self.make_path_files("")):
            if int(n) > self.id_seq:
                self.id_seq = int(n)
        self.id_seq += 1
        

    def make_path(self, *parts):
        return self.tagdb + os.path.sep + os.path.sep.join(parts)
    
    def make_path_tags(self, *parts):
        return self.make_path(*("tags",) + parts)
    
    def make_path_files(self, *parts):
        return self.make_path(*("files",) + parts)
    
    def make_path_map(self, *parts):
        return self.make_path(*("map",) + parts)

    def next_id(self):
        s = str(self.id_seq)
        self.id_seq += 1
        return s

    def read_file(self, id):
        f = open(self.make_path_files(id), "r")
        lines = f.readlines()
        f.close()
        l = [s.strip() for s in lines]
        return (l[0], l[1:])

    def write_file(self, id, path, tags):
        f = open(self.make_path_files(id), "w")
        f.write("\n".join([path] + tags))
        f.close()

    # TODO: this really needed? is not catched
    def tags_exist(self, tags):
        for tag in tags:
            if not os.path.exists(self.make_path_tags(tag)):
                raise TagDBTagDoesNotExist()

    def add_tag(self, name):
        path = self.make_path_tags(name)
        if os.path.isdir(path) or os.path.islink(self.make_path_map(name)):
            raise TagDBTagAlreadyExists()
        else:
            os.mkdir(path)

    def remove_tag(self, name):
        try:
            os.rmdir(self.make_path_tags(name))
        except OSError, e:
            if e.errno == errno.ENOTEMPTY:
                raise TagDBTagHasFiles()
            else:
                raise

    def add_file(self, name, dest_path, tags):
        # make sure all tags exist, else raise exception
        self.tags_exist(tags)

	if os.path.isdir(self.make_path_tags(name)):
	    raise TagDBFileAlreadyExists()

        path = self.make_path_map(name)
        # file already exists
        if os.path.islink(path):
            id = os.readlink(path)
            (file_path, file_tags) = self.read_file(id)
            # basename already exists with other path
            if dest_path != file_path:
                raise TagDBFileAlreadyExists()

            file_tags = list(set(file_tags).union(set(tags)))
            self.write_file(id, file_path, file_tags)

        # new file
        else:
            id = self.next_id()
            self.write_file(id, dest_path, tags)
            os.symlink(id, path)

        for tag in tags:
            if os.path.exists(self.make_path_tags(tag, id)):
                continue
            
            # touch file
            open(self.make_path_tags(tag, id), "w").close()

    def remove_file(self, name, tags):
        # make sure all tags exist, else raise exception
        self.tags_exist(tags)
        
        path = self.make_path_map(name)
        # file could already have been deleted
        try:
            id = os.readlink(path)
        except OSError, e:
            # someone already removed the file
            if e.errno == errno.ENOENT:
                raise TagDBFileDoesNotExist()
            else:
                raise

        for tag in tags:
            if not os.path.exists(self.make_path_tags(tag, id)):
                continue
            
            os.unlink(self.make_path_tags(tag, id))
        
        (file_path, file_tags) = self.read_file(id)
        file_tags = list(set(file_tags).difference(set(tags)))
        self.write_file(id, file_path, file_tags)

        # no tags left, remove file
        if file_tags == []:
            os.unlink(path)
            os.unlink(self.make_path_files(id))

    def intersection(self, tags):
        # make sure all tags exist, else raise exception
        self.tags_exist(tags)
        
        # no tags, return all tags
        if tags == []:
            return dircache.listdir(self.make_path_tags())
        else:
            s = set(dircache.listdir(self.make_path_tags(tags[0])))
            for tag in tags[1:]:
                s.intersection_update(dircache.listdir(self.make_path_tags(tag)))
       
	
        union = dict()
        files = []
        for id in s:
            (file_path, file_tags) = self.read_file(id)
	    for tag in file_tags:
		union[tag] = union.get(tag, 0) + 1
            files.append(os.path.basename(file_path))
	
	narrow = filter(lambda (tag, count): count != len(files), union.items())

	return (files, map(lambda (tag, count): tag, narrow))
       
	if 0:
	    # collect basename and union of all tags of matching files
	    union = set()
	    files = []
	    for id in s:
		(file_path, file_tags) = self.read_file(id)
		union.update(set(file_tags))
		files.append(os.path.basename(file_path))

	    # list of files and list of other tags the matched files belong to
	    return (files, list(union.difference(set(tags))))
    
    def get_link(self, name):
        path = self.make_path_map(name)
        try:
            id = os.readlink(path)
        except OSError, e:
            if e.errno == errno.ENOENT:
                raise TagDBFileDoesNotExist()
            else:
                raise
       
	(file_path, file_tags) = self.read_file(id)
	return file_path

    def get_file(self, name, tags):
        path = self.make_path_map(name)
        try:
	    s = os.lstat(path)
	    id = os.readlink(path)
        except OSError, e:
            if e.errno == errno.ENOENT:
                raise TagDBFileDoesNotExist()
            else:
                raise
    
	# does not exist if some tag is missing, add_file will
	# skip existing tags anyway
	for tag in tags:
	    if not os.path.isfile(self.make_path_tags(tag, id)):
		raise TagDBFileDoesNotExist()

	return s

    def get_tag(self, name):
	# TODO: this is ugly and maybe not really needed
	# fake st_nlink=3 for tags (directories) to make
	# find etc happy (> 2 means there are sub directories)
	# TODO: os.stat returns a readonly stat class
	class dummystat:
	    def __init__(self, s):
		self.st_atime = s.st_atime
		self.st_ctime = s.st_ctime
		self.st_ino = s.st_ino
		self.st_nlink = s.st_nlink
		self.st_uid = s.st_uid
		self.st_blksize = s.st_blksize
		self.st_dev = s.st_dev
		self.st_mode = s.st_mode
		self.st_rdev = s.st_rdev
		self.st_blocks = s.st_blocks
		self.st_gid = s.st_gid
		self.st_mtime = s.st_mtime
		self.st_size = s.st_size
        try:
            #return os.stat(self.make_path_tags(name))
            s = dummystat(os.stat(self.make_path_tags(name)))
	    s.st_nlink += 1
	    return s
        except OSError, e:
            if e.errno == errno.ENOENT:
                raise TagDBTagDoesNotExist()
            else:
                raise

   
    # do filesystem sanity check
    def fsck(self):
        for id in dircache.listdir(self.make_path_files()):
            try:
                (file_path, file_tags) = self.read_file(id)
            except OSError, e:
                print "files/%s: %s" % (id, e)
                continue
            except IndexError, e:
                print "files/%s: unknown file: %s" % (id, e)
                continue

            if not os.path.exists(file_path):
                print "files/%s: broken link: %s" % (id, file_path)
            
            basename = os.path.basename(file_path)

            if not os.path.islink(self.make_path_map(basename)):
                print "files:/%s: basename %s not found in map" % (id, basename)
            else:
                try:
                    link_id = os.readlink(self.make_path_map(basename))
                except OSError, e:
                    print "files/%s: failed to read map link: %s" (id, e)

                if id != link_id:
                    print "files/%s: id differ %s" % (id, link_id)

            for tag in file_tags:
                if not os.path.isdir(self.make_path_tags(tag)):
                    print "files/%s: tag %s does not exist" % (id, tag)
                if not os.path.isfile(self.make_path_tags(tag, id)):
                    print "files/%s: tag %s not in tag directory" % (id, tag)

        for tag in dircache.listdir(self.make_path_tags()):
            for id in dircache.listdir(self.make_path_tags(tag)):
                if not os.path.isfile(self.make_path_files(id)):
                    print "tags/%s/%s: does not match any file id" % (tag, id)

        for basename in dircache.listdir(self.make_path_map()):
            try:
                id = os.readlink(self.make_path_map(basename))
            except OSError, e:
                print "map/%s: failed to read link: %s" % (basename, e)
                continue
            
            if not os.path.isfile(self.make_path_files(id)):
                print "map/%s: broken file id %s" % (basename, id)

            try:
                (file_path, file_tags) = self.read_file(id)
            except OSError, e:
                print "map/%s: %s" % (basename, e)
                continue
            except IndexError, e:
                # if exception, it has already been reported above
                continue

            if basename != os.path.basename(file_path):
                print "map/%s: basename does not match %s" % (basename, file_path)



def test():

    db = TagDBFS(tagdb="tagdb")

    db.add_tag("tag1");
    db.add_tag("tag2");
    db.add_tag("tag3");
    db.add_file("file1", "/path/to/file1", ["tag1", "tag2"])
    db.add_file("file2", "/path/to/file1", ["tag1", "tag3"])
    db.add_file("file3", "/path/to/file1", ["tag1"])
    
    db.remove_file("file1", ["tag1"])
    db.add_file("file1", "/path/to/file1", ["tag1", "tag2"])
    
    
    try: db.add_file("file1", "/path/to/file1other", ["tag1", "tag2"])
    except TagDBFileAlreadyExists, e: print "good, already exists"

    print db.intersection([])
    print db.intersection(["tag1"])
    print db.intersection(["tag1", "tag2"])
    print db.intersection(["tag1", "tag3"])
    try: print db.intersection(["bla"])
    except TagDBTagDoesNotExist, e: print "good, tag not found"

    print db.get_file("file1")
    try: print db.get_file("bla")
    except TagDBFileDoesNotExist, e: print "good, file not found"
    
    print db.get_tag("tag1")
    try: print db.get_tag("bla")
    except TagDBTagDoesNotExist, e: print "good, tag not found"
    
    try: db.remove_tag("tag1")
    except TagDBTagHasFiles, e: print "good, tag should not be empty"
    
    db.add_tag("bla")
    try: db.remove_tag("bla")
    except TagDBTagHasFiles, e: print "bad, tag should not be empty"


def test2(tagdb, files, tags, avgadd):
    import random

    db = TagDBFS(tagdb=tagdb)

    for i in range(0, tags):
        db.add_tag("tag" + str(i));
    
    for i in range(0, files):
        t = ["tag" + str(random.randrange(0, tags)) for r in range(0, avgadd)]
        db.add_file("file" + str(i), "/path/to/file" + str(i), t);


if __name__ == "__main__":
    import sys
    test2(sys.argv[1], int(sys.argv[2]), int(sys.argv[3]), int(sys.argv[4]))



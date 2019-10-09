
from distutils.core import setup, Extension
import sys

if sys.version_info[0] > 2:
    swig_opts = ['-py3']
else:
    swig_opts = ['-DPYTHON2']

# 'build_ext' must be run before 'build_py' to create 'corpustools.py'
for i in range(1, len(sys.argv)):
    if sys.argv[i] == 'build_ext':
        break
    if sys.argv[i] == 'build' or sys.argv[i] == 'build_py' or sys.argv[i].startswith('install'):
        sys.argv = sys.argv[:i] + ['build_ext'] + sys.argv[i:]
        break

corpustools_module = Extension('_corpustools',
                               sources = ['corpustools.c', 'libtok.c', 'corpustools.i'],
                               depends = ['corpustools.h'],
                               swig_opts = swig_opts,
                               )

setup(name = 'corpustools',
      version = '0.1',
      author = 'Peter Kleiweg',
      description = '''Tools for working with corpora''',
      ext_modules = [corpustools_module],
      py_modules = ['corpustools'],
      )

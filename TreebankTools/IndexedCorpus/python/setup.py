#!/usr/bin/python
# -*- coding: utf-8 -*-

import os
from distutils.core import setup, Extension

if not os.environ.has_key('BOOST_HOME'):
  raise "Please set BOOST_HOME"

BOOST_HOME = os.environ['BOOST_HOME']
BOOST_LIB = "%s/lib" % BOOST_HOME
BOOST_INCLUDE = "%s/include" % BOOST_HOME

m1 = Extension('indexedcorpus',
	include_dirs = ['../', BOOST_INCLUDE],
	library_dirs = [BOOST_LIB],
	runtime_library_dirs = [BOOST_LIB],
	libraries = ['z', 'boost_system', 'boost_filesystem','boost_thread'],
	extra_objects = ['../libcorpus.a'],
	language = 'c++',
	sources = ['indexedcorpus.cpp'])

setup(name = 'IndexedCorpus',
	version = '1.0',
	description = 'IndexedCorpus Python bindings',
	author = 'Daniël de Ko',
	author_email = 'me@danieldk.eu',
	ext_modules = [m1])

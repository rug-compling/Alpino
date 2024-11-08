#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
from setuptools import setup, Extension

if 'ALPINO_HOME' not in os.environ:
    raise RuntimeError("Please set ALPINO_HOME")

if 'BOOST_HOME' not in os.environ:
    raise RuntimeError("Please set BOOST_HOME")

ALPINO_HOME = os.environ['ALPINO_HOME']

BOOST_HOME = os.environ['BOOST_HOME']
BOOST_LIB = "%s/lib" % BOOST_HOME
BOOST_INCLUDE = "%s/include" % BOOST_HOME

m1 = Extension('indexedcorpus',
	include_dirs = ['%s/TreebankTools/IndexedCorpus' % ALPINO_HOME, BOOST_INCLUDE],
	library_dirs = [BOOST_LIB],
	runtime_library_dirs = [BOOST_LIB],
	libraries = ['z', 'boost_system', 'boost_filesystem'],
	extra_objects = ['%s/TreebankTools/IndexedCorpus/libcorpus.a' % ALPINO_HOME],
	language = 'c++',
	sources = ['indexedcorpus.cpp'])

setup(name = 'IndexedCorpus',
	version = '1.0',
	description = 'IndexedCorpus Python bindings',
	author = 'Daniël de Kok',
	author_email = 'me@danieldk.eu',
	ext_modules = [m1])

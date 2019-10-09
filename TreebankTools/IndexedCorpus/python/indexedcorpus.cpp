#include <Python.h>

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include <boost/smart_ptr/shared_ptr.hpp>

#include <IndexedCorpus/ActCorpusReader.hh>
#include <IndexedCorpus/DzIstream.hh>
#include <IndexedCorpus/DzOstream.hh>
#include <IndexedCorpus/IndexedCorpusReader.hh>
#include <IndexedCorpus/IndexedCorpusWriter.hh>

void raise_exception(char const *msg)
{
	//PyErr_SetString(PyExc_RuntimeError, msg);
	PyObject *msgObj = Py_BuildValue("s", msg);
	PyErr_SetObject(PyExc_RuntimeError, msgObj);
}

/*
 * IndexedCorpusReader
 */

typedef struct {
	PyObject_HEAD
	indexedcorpus::IndexedCorpusReader *indexedCorpusReader;
} IndexedCorpusReader;

// CorpusReader allocation
static PyObject *IndexedCorpusReader_new(PyTypeObject *type, PyObject *args,
	PyObject *kwds)
{
	char *dataFilename;
	char *indexFilename;

	if (!PyArg_ParseTuple(args, "ss", &indexFilename, &dataFilename))
		return NULL;
	
	IndexedCorpusReader *self;
	
	self = (IndexedCorpusReader *) type->tp_alloc(type, 0);
	if (self == NULL)
		return NULL;

	boost::shared_ptr<std::istream> dataStream;
	try {
		dataStream.reset(new indexedcorpus::DzIstream(dataFilename));
	} catch (std::exception &e) {
		raise_exception(e.what());
		return NULL;
	}

	if (!*dataStream)
	{
		raise_exception("Could not open data file!");
		return NULL;
	}

	boost::shared_ptr<std::istream> indexStream(new std::ifstream(indexFilename));
	if (!*indexStream) {
		raise_exception("Could not open index file!");
		return NULL;
	}

	self->indexedCorpusReader =
		new indexedcorpus::IndexedCorpusReader(dataStream, indexStream);
	
	return (PyObject *) self;	
}

// IndexedCorpusReader deallocation
static void IndexedCorpusReader_dealloc(IndexedCorpusReader *self)
{
	delete self->indexedCorpusReader;
	self->ob_type->tp_free((PyObject *) self);
}

// Retrieve data from a corpus.
static PyObject *IndexedCorpusReader_data(IndexedCorpusReader *self, PyObject *args)
{
	char *name;
	if (!PyArg_ParseTuple(args, "s", &name))
		return NULL;

	std::vector<unsigned char> data;
	try {
		data = self->indexedCorpusReader->read(name);
	} catch (std::exception &e) {
		raise_exception(e.what());
		return NULL;
	}

	return Py_BuildValue("s#", &data[0], data.size());
}

static PyObject *IndexedCorpusReader_entries(IndexedCorpusReader *self)
{
	
	PyObject *entries = PyList_New(0);
	if (entries == NULL)
		return NULL;
	
	std::vector<std::string> const &entryVec = self->indexedCorpusReader->entries();
	for (std::vector<std::string>::const_iterator iter = entryVec.begin();
		iter != entryVec.end(); ++iter)
	{
		PyObject *item = Py_BuildValue("s", iter->c_str());
		if (item == NULL || PyList_Append(entries, item) < 0)
		{
			Py_DECREF(item);
			return NULL;
		}
		Py_DECREF(item);
	}
	
	return entries;
}

static PyMethodDef IndexedCorpusReader_methods[] = {
	{ "data", (PyCFunction) IndexedCorpusReader_data, METH_VARARGS, "Get corpus data" },
	{ "entries", (PyCFunction) IndexedCorpusReader_entries, METH_NOARGS, "List corpus entries" },
	{NULL} /* Sentinel */
};

static PyTypeObject IndexedCorpusReaderType = {
	    PyObject_HEAD_INIT(NULL)
	    0,                                        /* ob_size */
	    "indexedcorpus.IndexedCorpusReader",      /* tp_name */
	    sizeof(IndexedCorpusReader),              /* tp_basicsize */
	    0,                                        /* tp_itemsize */
	    (destructor)IndexedCorpusReader_dealloc,  /* tp_dealloc */
	    0,                                        /* tp_print */
	    0,                                        /* tp_getattr */
	    0,                                        /* tp_setattr */
	    0,                                        /* tp_compare */
	    0,                                        /* tp_repr */
	    0,                                        /* tp_as_number */
	    0,                                        /* tp_as_sequence */
	    0,                                        /* tp_as_mapping */
	    0,                                        /* tp_hash */
	    0,                                        /* tp_call */
	    0,                                        /* tp_str */
	    0,                                        /* tp_getattro */
	    0,                                        /* tp_setattro */
	    0,                                        /* tp_as_buffer */
	    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /* tp_flags */
	    "IndexedCorpusReader objects",            /* tp_doc */
	    0,		                                  /* tp_traverse */
	    0,                                        /* tp_clear */
	    0,		                                  /* tp_richcompare */
	    0,		                                  /* tp_weaklistoffset */
	    0,		                                  /* tp_iter */
	    0,		                                  /* tp_iternext */
	    IndexedCorpusReader_methods,              /* tp_methods */
	    0,                                        /* tp_members */
	    0,                                        /* tp_getset */
	    0,                                        /* tp_base */
	    0,                                        /* tp_dict */
	    0,                                        /* tp_descr_get */
	    0,                                        /* tp_descr_set */
	    0,                                        /* tp_dictoffset */
	    0,                                        /* tp_init */
	    0,                                        /* tp_alloc */
	    IndexedCorpusReader_new,                  /* tp_new */
	};

/*
 * IndexedCorpusWriter
 */

typedef struct {
	PyObject_HEAD
	indexedcorpus::IndexedCorpusWriter *indexedCorpusWriter;
} IndexedCorpusWriter;

// CorpusReader allocation
static PyObject *IndexedCorpusWriter_new(PyTypeObject *type, PyObject *args,
	PyObject *kwds)
{
	char *dataFilename;
	char *indexFilename;

	if (!PyArg_ParseTuple(args, "ss", &indexFilename, &dataFilename))
		return NULL;

	IndexedCorpusWriter *self;

	self = (IndexedCorpusWriter *) type->tp_alloc(type, 0);
	if (self == NULL)
		return NULL;

	boost::shared_ptr<std::ostream> dataStream;
	try {
		dataStream.reset(new indexedcorpus::DzOstream(dataFilename));
	} catch (std::exception &e) {
		raise_exception(e.what());
		return NULL;
	}

	if (!*dataStream)
	{
		raise_exception("Could not open data file!");
		return NULL;
	}

	boost::shared_ptr<std::ostream> indexStream(new std::ofstream(indexFilename));
	if (!*indexStream) {
		raise_exception("Could not open index file!");
		return NULL;
	}

	self->indexedCorpusWriter =
		new indexedcorpus::IndexedCorpusWriter(dataStream, indexStream);

	return (PyObject *) self;	
}

// IndexedCorpusWriter deallocation
static void IndexedCorpusWriter_dealloc(IndexedCorpusWriter *self)
{
	delete self->indexedCorpusWriter;
	self->ob_type->tp_free((PyObject *) self);
}

static PyObject *IndexedCorpusWriter_write(IndexedCorpusWriter *self, PyObject *args)
{
	char *name;
	char *data;
	if (!PyArg_ParseTuple(args, "ss", &name, &data)) {
		raise_exception("Incorrect number or type of arguments!");
		return NULL;
	}
	
	try {
		self->indexedCorpusWriter->write(name, data);
	} catch (std::exception &e) {
		raise_exception(e.what());
		return NULL;
	}
	
	Py_INCREF(Py_None);
	return Py_None;
	
}

static PyMethodDef IndexedCorpusWriter_methods[] = {
	{ "write", (PyCFunction) IndexedCorpusWriter_write, METH_VARARGS, "Write corpus data" },
	{NULL} /* Sentinel */
};

static PyTypeObject IndexedCorpusWriterType = {
	    PyObject_HEAD_INIT(NULL)
	    0,                                        /* ob_size */
	    "indexedcorpus.IndexedCorpusWriter",      /* tp_name */
	    sizeof(IndexedCorpusWriter),              /* tp_basicsize */
	    0,                                        /* tp_itemsize */
	    (destructor)IndexedCorpusWriter_dealloc,  /* tp_dealloc */
	    0,                                        /* tp_print */
	    0,                                        /* tp_getattr */
	    0,                                        /* tp_setattr */
	    0,                                        /* tp_compare */
	    0,                                        /* tp_repr */
	    0,                                        /* tp_as_number */
	    0,                                        /* tp_as_sequence */
	    0,                                        /* tp_as_mapping */
	    0,                                        /* tp_hash */
	    0,                                        /* tp_call */
	    0,                                        /* tp_str */
	    0,                                        /* tp_getattro */
	    0,                                        /* tp_setattro */
	    0,                                        /* tp_as_buffer */
	    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /* tp_flags */
	    "IndexedCorpusWriter objects",            /* tp_doc */
	    0,		                                  /* tp_traverse */
	    0,                                        /* tp_clear */
	    0,		                                  /* tp_richcompare */
	    0,		                                  /* tp_weaklistoffset */
	    0,		                                  /* tp_iter */
	    0,		                                  /* tp_iternext */
	    IndexedCorpusWriter_methods,              /* tp_methods */
	    0,                                        /* tp_members */
	    0,                                        /* tp_getset */
	    0,                                        /* tp_base */
	    0,                                        /* tp_dict */
	    0,                                        /* tp_descr_get */
	    0,                                        /* tp_descr_set */
	    0,                                        /* tp_dictoffset */
	    0,                                        /* tp_init */
	    0,                                        /* tp_alloc */
	    IndexedCorpusWriter_new,                  /* tp_new */
	};

/*
 * CorpusReader class: provides an ActCorpusReader interface for Python.
 */

typedef struct {
	PyObject_HEAD
	indexedcorpus::ActCorpusReader *corpusReader;
} CorpusReader;

// CorpusReader allocation
static PyObject *CorpusReader_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
	CorpusReader *self;
	
	self = (CorpusReader *) type->tp_alloc(type, 0);
	if (self != NULL)
		self->corpusReader = new indexedcorpus::ActCorpusReader;
	
	return (PyObject *) self;	
}

// CorpusReader deallocation
static void CorpusReader_dealloc(CorpusReader *self)
{
	delete(self->corpusReader);
	self->ob_type->tp_free((PyObject *) self);
}

// Retrieve data from a corpus.
static PyObject *CorpusReader_data(CorpusReader *self, PyObject *args)
{
	char *name;
	if (!PyArg_ParseTuple(args, "s", &name))
		return NULL;

	std::vector<unsigned char> data;
	try {
		data = self->corpusReader->getData(name);
	} catch (std::exception &e) {
		raise_exception(e.what());
		return NULL;
	}
	
	return Py_BuildValue("s#", &data[0], data.size());
}

static PyObject *CorpusReader_pathName(CorpusReader *self, PyObject *args)
{
	char *name;
	int offset;
	if (!PyArg_ParseTuple(args, "si", &name, &offset))
		return NULL;
	
	std::string pathName;
	try {
		pathName = self->corpusReader->pathName(name, offset);
	} catch (std::exception &e) {
		raise_exception(e.what());
		return NULL;
	}
	
	return Py_BuildValue("s", pathName.c_str());
}

static PyMethodDef CorpusReader_methods[] = {
	{ "data", (PyCFunction) CorpusReader_data, METH_VARARGS,
		"Retrieve data from a corpus" },
	{ "pathName", (PyCFunction) CorpusReader_pathName, METH_VARARGS,
		"Find a path relative to the current path"},
	{NULL} /* Sentinel */
};

static PyTypeObject CorpusReaderType = {
	    PyObject_HEAD_INIT(NULL)
	    0,                                        /* ob_size */
	    "indexedcorpus.CorpusReader",             /* tp_name */
	    sizeof(CorpusReader),                     /* tp_basicsize */
	    0,                                        /* tp_itemsize */
	    (destructor)CorpusReader_dealloc,         /* tp_dealloc */
	    0,                                        /* tp_print */
	    0,                                        /* tp_getattr */
	    0,                                        /* tp_setattr */
	    0,                                        /* tp_compare */
	    0,                                        /* tp_repr */
	    0,                                        /* tp_as_number */
	    0,                                        /* tp_as_sequence */
	    0,                                        /* tp_as_mapping */
	    0,                                        /* tp_hash */
	    0,                                        /* tp_call */
	    0,                                        /* tp_str */
	    0,                                        /* tp_getattro */
	    0,                                        /* tp_setattro */
	    0,                                        /* tp_as_buffer */
	    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /* tp_flags */
	    "CorpusReader objects",                   /* tp_doc */
	    0,		                                  /* tp_traverse */
	    0,                                        /* tp_clear */
	    0,		                                  /* tp_richcompare */
	    0,		                                  /* tp_weaklistoffset */
	    0,		                                  /* tp_iter */
	    0,		                                  /* tp_iternext */
	    CorpusReader_methods,                     /* tp_methods */
	    0,                                        /* tp_members */
	    0,                                        /* tp_getset */
	    0,                                        /* tp_base */
	    0,                                        /* tp_dict */
	    0,                                        /* tp_descr_get */
	    0,                                        /* tp_descr_set */
	    0,                                        /* tp_dictoffset */
	    0,                                        /* tp_init */
	    0,                                        /* tp_alloc */
	    CorpusReader_new,                         /* tp_new */
	};

static PyMethodDef IndexedCorpusMethods[] = {
	{NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC initindexedcorpus(void)
{
	PyObject *m;
	
	if (PyType_Ready(&CorpusReaderType) < 0)
		return;

	if (PyType_Ready(&IndexedCorpusReaderType) < 0)
		return;

	if (PyType_Ready(&IndexedCorpusWriterType) < 0)
		return;

	m = Py_InitModule("indexedcorpus", IndexedCorpusMethods);
    if (m == NULL)
        return;

	Py_INCREF(&CorpusReaderType);
	PyModule_AddObject(m, "CorpusReader", (PyObject *) &CorpusReaderType);
	Py_INCREF(&IndexedCorpusReaderType);
	PyModule_AddObject(m, "IndexedCorpusReader", (PyObject *) &IndexedCorpusReaderType);
	Py_INCREF(&IndexedCorpusWriterType);
	PyModule_AddObject(m, "IndexedCorpusWriter", (PyObject *) &IndexedCorpusWriterType);
}

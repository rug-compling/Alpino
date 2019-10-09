/*
  XQClient.java: een minimale XQuery processor die gebruikt maakt
  van de Saxon internals en bestanden ophaalt van een treebank server.

  XQClient.java  [-h host] [-p port] <queryfile|{query}> <--stdin|filename(s)>

  Geert Kloosterman <G.J.Kloosterman@rug.nl>
  Sat Dec  2 10:30:01 2006
*/


import net.sf.saxon.*;
import net.sf.saxon.query.*;

import net.sf.saxon.trans.XPathException;
import javax.xml.transform.stream.StreamSource;
import javax.xml.transform.stream.StreamResult;
import java.io.*;
import java.util.Properties;
import net.sf.saxon.om.DocumentInfo;

import net.sf.saxon.instruct.TerminationException;

import javax.xml.transform.OutputKeys;

/* 
 * This will break portability, but there seems to be
 * no good alternative... 
 * There's some documentation on these packages on
 * http://forum.java.sun.com/thread.jspa?threadID=514860&messageID=2451429
 */
import sun.misc.SignalHandler;
import sun.misc.Signal;

public class XQClient
{
    static String name = "xqclient";

    public static void main(String args[])
    {
        String queryFileName;

        String host = "localhost";
        int port = 44444;

        boolean useStdin = false;
        boolean textMode = false;
        boolean omitDecl = false;
        boolean latin1   = false;

        
        /* 
         * We use the undocumented sun.misc.Signal and
         * sun.misc.SignalHandler classes to trap SIGPIPE.
         *
         * This will work only with Sun JVMs.
         * GJK Sun Dec  3 12:18:39 2006
         */
        Signal.handle(new Signal("PIPE"), SignalHandler.SIG_DFL);

        int i = 0;
        while (i < args.length) {
            if (args[i].charAt(0) == '-') {
                // -h --host 
                // -p --port
                // -s --stdin
                // --omit-xml-declaration ???
                // -t --text                 ???
                if (args[i].equals("--host") || args[i].equals("-h")) {
                    i++;
                    if (args.length < i + 1) {
                        badUsage(name, "No host given");
                    }
                    host = args[i++];
                } else if (args[i].equals("--port") || args[i].equals("-p")) {
                    i++;
                    if (args.length < i + 1) {
                        badUsage(name, "No host given");
                    }
                    port = Integer.parseInt(args[i++]);
                } else if (args[i].equals("--stdin") || args[i].equals("-s")) {
                    useStdin = true;
                    i++;
                } else if (args[i].equals("--text") || args[i].equals("-t")) {
                    textMode = true;
                    i++;
                } else if (args[i].equals("--latin1") || args[i].equals("-l")) {
                    latin1 = true;
                    i++;
                } else if (args[i].equals("--omit-xml-decl")) {
                    omitDecl = true;
                    i++;
                } else {
                    badUsage(name, "Error: unsupported option \"" + args[i] + "\"!");
                }
            } else {
                break;
            }
        }

        if (args.length < i + 1) {
            badUsage(name, "Error: no query file given!");
        }
        queryFileName = args[i++];

        if (args.length < i + 1 && !useStdin) {
            badUsage(name, "Error: no files to process!");
        }


        Configuration config = new Configuration();
        StaticQueryContext staticContext = new StaticQueryContext(config);
        DynamicQueryContext dynamicContext = new DynamicQueryContext(config);

        XQueryExpression exp = null;

        // compileer de query

        try {

            if (queryFileName.startsWith("{") && queryFileName.endsWith("}")) {
                // query is inline on the command line
                String q = queryFileName.substring(1, queryFileName.length() - 1);
                exp = staticContext.compileQuery(q);
            } else {
                InputStream queryStream = new FileInputStream(queryFileName);
                staticContext.setBaseURI(new File(queryFileName).toURI().toString());
                exp = staticContext.compileQuery(queryStream, null);
            }
            staticContext = exp.getStaticContext();     // the original staticContext is copied


        } catch (XPathException err) { // this code taken from saxons Query.java
            int line = -1;
            String module = null;
            if (err.getLocator() != null) {
                line = err.getLocator().getLineNumber();
                module = err.getLocator().getSystemId();
            }
            if (err.hasBeenReported()) {
                quit("Failed to compile query", 2);
            } else {
                if (line == -1) {
                    System.err.println("Failed to compile query: " + err.getMessage());
                } else {
                    System.err.println("Static error at line " + line + " of " + module + ':');
                    System.err.println(err.getMessage());
                }
            }
            exp = null;
            System.exit(2);
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(2);
        }


        // pas 'm ergens op toe

        Properties props = new Properties();

        if (omitDecl)
            props.setProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
        if (textMode)
            props.setProperty(OutputKeys.METHOD, "text");

        if (latin1)
            props.setProperty(OutputKeys.ENCODING, "iso-8859-1");

        TreebankClient client = null;
        try {
            client = new TreebankClient(host, port);
        }
        catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }

        if (useStdin) {

            BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
            String line;
            String fileName = "";

            try {
                while ((line = reader.readLine()) != null) {
                    fileName = line;
                    processFile(client, fileName, exp, props, staticContext, dynamicContext);
                }
            } catch (IOException e) {
                e.printStackTrace();
                System.exit(1);
            }

        } else {
            String fileName = "";
            for (int arg = i; arg < args.length; arg++) {
                fileName = args[arg];
                processFile(client, fileName, exp, props, staticContext, dynamicContext);
            }
        }
        client.close();
    }

    protected static void processFile(TreebankClient client, 
                                      String fileName, 
                                      XQueryExpression exp, 
                                      Properties props,
                                      StaticQueryContext staticContext,
                                      DynamicQueryContext dynamicContext) {
        try { 
            ByteArrayInputStream bs = null;
            bs = client.getByteStream( fileName );
            if (bs == null) {
                System.err.println("Could not get data for \"" + fileName + "\",  Skipping...");
                return;
            }
            StreamSource ss = new StreamSource(bs);
            ss.setSystemId(fileName);
            DocumentInfo doc = staticContext.buildDocument(ss);

            dynamicContext.setContextItem(doc);

            exp.run(dynamicContext, new StreamResult(System.out), props);

        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        } catch (TerminationException err) {
            quit(err.getMessage(), 1);
        } catch (XPathException err) {
            // warn(name, "Query processing failed, skipping file: " + err.getMessage());
            // The error message is already printed at a higher level
            warn(name, "Query processing failed for \"" + fileName + "\", skipping...");
        }
    }

    protected static void warn(String programName, String message) {
        System.err.println(programName + ": Warning: " + message);
    }

    // taken from saxon
    protected static void quit(String message, int code) {
        System.err.println(message);
        System.exit(code);
    }
    protected static void badUsage(String name, String message) {
        if (!"".equals(message)) {
            System.err.println(name + ": " + message);
        }
        //System.err.println(config.getProductTitle());
        System.err.println("Usage: " + name + " [options] <queryfile|{query}> [filename(s)]");
        System.err.println("\nOptions: ");
        System.err.println("  -h, --host hostname   host name from treebankserver (default localhost)");
        System.err.println("  -p, --port port       port number from treebankserver (default 44444)");
        System.err.println("  -s, --stdin           read file ids from stdin");
        System.err.println("  -t, --text            output only text nodes");
        System.err.println("  -l, --latin1          set output encoding to iso-8859-1");
        System.err.println("  --omit-xml-decl       omit the xml declaration");
        if ("".equals(message)) {
            System.exit(0);
        } else {
            System.exit(2);
        }
    }
}

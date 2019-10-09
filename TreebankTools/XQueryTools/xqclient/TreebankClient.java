/*
  TreebankClient.java - Connect to an Alpino Treebank Server

  Author: Geert Kloosterman <G.J.Kloosterman@rug.nl>
  Date: Thu Nov 30 11:52:22 2006
*/


import java.io.*;
import java.net.*;


public class TreebankClient
{
    private Socket sock;
    private BufferedInputStream fromServer;
    private PrintWriter toServer;

    public TreebankClient(String host, int port) throws IOException
    {
        // This is needed on the new debian machines;
        // Without it we'll have to wait three minutes before a
        // socket is created.
        // See http://forum.java.sun.com/thread.jspa?threadID=5203917
        System.setProperty("java.net.preferIPv4Stack", "true");

        InetAddress addr = InetAddress.getByName(host);
        SocketAddress sockaddr = new InetSocketAddress(addr, port);
    
        // Create an unbound socket
        sock = new Socket();

        // This method will block no more than timeoutMs.
        // If the timeout occurs, SocketTimeoutException is thrown.
        int timeoutMs = 2000;   // 2 seconds

        sock.connect(sockaddr, timeoutMs);

        toServer = new PrintWriter(sock.getOutputStream(), true /* autoflush */);

        fromServer = new BufferedInputStream(sock.getInputStream());
    }

    /**
     * Close the connections to the treebank server
     */
    public void close()
    {
        try {
            toServer.close();
            fromServer.close();
        } 
        catch (IOException e) {
            // squish...
        }
    }


    /**
     * Return the data corresponding to fileName
     *
     * Returns null when the file doesn't exist
     */
    public byte[] getData(String fileName) throws IOException
    {

        toServer.println(fileName);

        int size = Integer.parseInt(getLine(fromServer));

        if (size == 0)
            return null;

        byte [] data = new byte[size];

        int ntoread = size;
        int offset = 0;

        while (true) {
            int nread = fromServer.read(data, offset, ntoread);
            
            if (nread == -1)
                throw new IOException("Unexpected EOF");

            ntoread -= nread;
            
            if (ntoread == 0)
                break;

            offset += nread;
            
            // System.err.println("DEBUG: now trying to read the rest of the bytes");
        }

        return data;
    }

    // FIXME : hoort dit hier wel???
    public ByteArrayInputStream getByteStream(String fileName) throws IOException
    {
        try {
            byte [] data = getData(fileName);
            if (data == null)
                return null;

            return new ByteArrayInputStream(data);            

        } catch (IOException e) {
            throw e;
        }
    }



    /**
     * Get a line from stream (without newline character)
     */
    private String getLine(InputStream stream) throws IOException
    {
        int c;

        String result = "";

        while ((c = stream.read()) != -1) {
            if (c == '\n')
                break;
            result += (char) c;
        }

        return result;
    }


    public static void main (String args[])
    {
        old_test();
    }

    public static void old_test()
    {
        byte [] data;

        String filename;

        try {

            TreebankClient client = new TreebankClient("localhost", 44444);

            String [] filenames  = {
                "/users2/geertk/compact-corpora/cdb/1.xml",
                "cdb/1.xml",
                "cdb/2119.xml" 
            };

            for (int i = 0; i < filenames.length; i++) {
                filename = filenames[i];

                data = client.getData(filename);
        
                if (data != null) {
                    System.out.println(filename + ":");
                    System.out.print(new String(data));
                } else {
                    System.err.println("Could not get data for \"" 
                                       + filename + "\"");
                }
            }
            client.close();

        }
        catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }
    }


    public static void new_test()
    {
        byte [] data;

        String filename;

        try {

            TreebankClient client = new TreebankClient("localhost", 44444);

            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));

            while ((filename = in.readLine()) != null) {

                data = client.getData(filename);
        
                if (data == null) {
                    System.err.println("Could not get data for \"" 
                                       + filename + "\"");
                } else {
                    System.out.println(filename);
                    // System.out.println(filename + ":");
                    // System.out.print(new String(data));
                }
            }
            client.close();

        }
        catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }
    }
}

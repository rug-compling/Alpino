/*
  MyUriResolver.java - kijken of we een URI kunnen resolven met de TreebankClient

  Author: Geert Kloosterman <G.J.Kloosterman@rug.nl>
  Date: Thu Nov 30 15:03:25 2006
*/

import java.io.*;
import javax.xml.transform.Source;
import javax.xml.transform.TransformerException;
import javax.xml.transform.URIResolver;
import javax.xml.transform.stream.StreamSource;

public class MyUriResolver implements URIResolver
{
    
    private TreebankClient client;

    public MyUriResolver() {
        try {
            client = new TreebankClient("localhost", 44444);
        }
        catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }
    }


    public Source resolve(String href, String base) throws TransformerException {

        byte [] data = null;
        StreamSource ss = null;

        // System.err.println("Trying to 'resolve' " + href);

        if (href.startsWith("file:")) {
            href = href.substring(5);
        }

        try {
            data = client.getData(href);
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }


        if (data == null) {
            System.err.println("Yuk!  Got some empty data here!!!");
            return null;
        }


        ByteArrayInputStream is = new ByteArrayInputStream(data);

        // System.err.println("Apparently i've got some data!!!");
        ss = new StreamSource(is);
        if (ss == null)
            System.err.println("The stream source is empty!!! (dammit)");

        ss.setSystemId(href);

        return  ss;

        // throw new TransformerException("Yeah!!!!  href=\"" + href + "\" base=\"" + base + "\"");

    }
}



/*
Local Variables:
compile-command: "javac -classpath /users1/gosse/src/saxon8-8/saxon8.jar:. MyUriResolver.java"
End:
*/

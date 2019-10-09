#!/usr/bin/perl -w

# TreebankClient.pm  -- Contact an Alpino Treebank Server

# Use "perldoc <this file>" to see the full documentation


use strict;
use IO::Socket;

package TreebankClient;

our $host = "roskva.let.rug.nl";
our $port = 44444;
our $socket;


sub open_connection {
    my ($host, $port) = @_;
    $socket = new IO::Socket::INET ( PeerAddr => $host,
                                     PeerPort => $port,
                                     Proto => 'tcp',
                                     );
    die "Could not create socket: $!\n" unless $socket;
}


sub close_connection {
    $socket->close;
}


sub get_data {
    my $file = $_[0];

    # schrijf de bestandsnaam
    $socket->printf("%s\n", $file);

    # haal de grootte op
    my $size = $socket->getline;

    # lees het benodigde aantal bytes
    my $data;

    $socket->read($data, $size);

    return $data;
}

1;

__END__

=head1 NAME

TreebankClient - Contact an Alpino Treebank Server

=head1 SYNOPSIS

  use TreebankClient;

  TreebankClient::open_connection('roskva.let.rug.nl', 44444);

  $data = TreebankClient::get_data('cdb/1.xml');

  # do something with $data

  # possibly get more data from the server

  TreebankClient::close_connection;


=head1 DESCRIPTION

This module provides three functions to access a treebank server:

=over 4

=item open_connection()

Use open_connection(host,port) to start a connection to the
treebank server.

=item get_data()

Use get_data("path/to/file.xml") to get the specified xml data from
the treebank server.

When the file doesn't exist an empty string is returned.

=item close_connection()

Use close_connection() to close the connection to the
treebank server.

=back

=head1 AUTHOR

Geert Kloosterman <G.J.Kloosterman@rug.nl>.

=cut

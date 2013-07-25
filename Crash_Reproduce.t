#!/usr/bin/perl

use strict;
use warnings;

use Carp ();

package My::W3C::SOAP::Document;

# Created on: 2012-05-27 19:26:43
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use XML::LibXML;

has xml => (
    is       => 'ro',
    isa      => 'XML::LibXML::Document',
    required => 1,
);
has xpc => (
    is         => 'ro',
    isa        => 'XML::LibXML::XPathContext',
    builder    => '_xpc',
    lazy_build => 1,
);

sub _xpc {
    my ($self) = @_;
    my $xpc = XML::LibXML::XPathContext->new($self->xml);
    $xpc->registerNs(xs   => 'http://www.w3.org/2001/XMLSchema');
    $xpc->registerNs(xsd  => 'http://www.w3.org/2001/XMLSchema');
    $xpc->registerNs(wsdl => 'http://schemas.xmlsoap.org/wsdl/');
    $xpc->registerNs(soap => 'http://schemas.xmlsoap.org/wsdl/soap/');

    return $xpc;
}

package My::W3C::SOAP::Document::Node;

# Created on: 2012-05-26 19:04:19
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;

has node => (
    is       => 'rw',
    isa      => 'XML::LibXML::Node',
    required => 1,
);
has document => (
    is         => 'rw',
    isa        => 'My::W3C::SOAP::Document',
    required   => 1,
    weak_ref   => 1,
    handles    => {
        xpc => 'xpc',
    },
);
has name => (
    is         => 'rw',
    isa        => 'Maybe[Str]',
    builder    => '_name',
    lazy_build => 1,
);

sub _name {
    my ($self) = shift;
    return $self->node->getAttribute('name');
}

package My::W3C::SOAP::XSD::Document;

# Created on: 2012-05-26 15:46:31
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;

extends 'My::W3C::SOAP::Document';


has simple_types => (
    is         => 'rw',
    isa        => 'ArrayRef[My::W3C::SOAP::Document::Node]',
    builder    => '_simple_types',
    lazy_build => 1,
);
has simple_type => (
    is         => 'rw',
    isa        => 'HashRef[My::W3C::SOAP::Document::Node]',
    builder    => '_simple_type',
    lazy_build => 0,
);
has anon_simple_type_count => (
    is      => 'ro',
    isa     => 'Int',
    traits  => [qw/Counter/],
    default => -1,
    handles => { simple_type_count => 'inc' },
);

sub _simple_types {
    my ($self) = @_;
    my @simple_types;
    my @nodes = $self->xpc->findnodes('//xsd:simpleType');

    for my $node (@nodes) {
        push @simple_types, My::W3C::SOAP::Document::Node->new(
            document => $self,
            node   => $node,
        );
    }

    return \@simple_types;
}

sub _simple_type {
    my ($self) = @_;
    my %simple_type;

    for my $type (@{ $self->simple_types }) {
        my $name = $type->name;
        if ( !$name ) {
            my $parent = $type->node->parentNode;
            $name = $parent->getAttribute('name');
            $name = $name ? $name . '_type' : 'anon'.$self->simple_type_count;
            $type->name($name);
        }
        Carp::confess "No name for simple type ".$type->node->parentNode->toString if !$name;
        $simple_type{$name} = $type;
    }

    return \%simple_type;
}

package My::W3C::SOAP::WSDL::Document;

# Created on: 2012-05-27 18:57:29
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;

extends 'My::W3C::SOAP::Document';


has messages => (
    is         => 'rw',
    isa        => 'ArrayRef[My::W3C::SOAP::Document::Node]',
    builder    => '_messages',
    lazy_build => 1,
);
has schemas => (
    is         => 'rw',
    isa        => 'ArrayRef[My::W3C::SOAP::XSD::Document]',
    builder    => '_schemas',
    lazy_build => 1,
);

sub _messages {
    my ($self) = @_;
    my @messages;
    my @nodes = $self->xpc->findnodes('//wsdl:message');

    for my $node (@nodes) {
        push @messages, My::W3C::SOAP::Document::Node->new(
            document => $self,
            node   => $node,
        );
    }

    return \@messages;
}

sub _schemas {
    my ($self) = @_;
    my @schemas;
    my @nodes = $self->xpc->findnodes('//wsdl:types/*');

    for my $node (@nodes) {
        next if $node->getAttribute('namespace') && $node->getAttribute('namespace') eq 'http://www.w3.org/2001/XMLSchema';

        # merge document namespaces into the schema's tags
        my $doc = $self->xml->getDocumentElement;
        my @attribs = $doc->getAttributes;
        for my $ns ( grep {$_->name =~ /^xmlns:/ && !$node->getAttribute($_->name)} @attribs ) {
            $node->setAttribute( $ns->name, 'value' );
            $node->setAttribute( $ns->name, $ns->value );
        }

        push @schemas, My::W3C::SOAP::XSD::Document->new(
            xml => XML::LibXML->load_xml(
                string => $node->toString,
            ),
        );
    }

    return \@schemas;
}

package main;

use Test::More;

# create the parser object
my $doc = My::W3C::SOAP::WSDL::Document->new(
    xml => XML::LibXML->load_xml(
        location      => 't/eg.wsdl',
    ),
);

ok $doc, "Got a parser object";
ok scalar( @{ $doc->messages }      ), "Got some messages";
ok scalar( @{ $doc->schemas }  ), "Got some schemas";

done_testing();
exit;

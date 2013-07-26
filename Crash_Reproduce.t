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

sub simple_type_count
{
    my ($self) = @_;

    if (!exists($self->{anon_simple_type_count})) {
        $self->{anon_simple_type_count} = -1;
    }

    return ++$self->{anon_simple_type_count};
}

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

use vars qw(@ISA);

@ISA=('My::W3C::SOAP::Document');

sub messages {
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

sub schemas {
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
        string => <<'EOF',
<?xml version="1.0" encoding="UTF-8"?>
<wsdl:definitions
    name="EgService"
    targetNamespace="http://eg.schema.org/v1"
    xmlns:wssec="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd"
    xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/"
    xmlns:eg="http://eg.schema.org/v1"
    xmlns:wsdlsoap="http://schemas.xmlsoap.org/wsdl/soap/"
    xmlns:wsp="http://schemas.xmlsoap.org/ws/2004/09/policy"
    >
  <wsp:UsingPolicy wsdl:Required="true"/>
  <wsp:Policy wssec:Id="Auth.xml">
    <wssp:Identity xmlns:wssp="http://www.bea.com/wls90/security/policy">
      <wssp:SupportedTokens>
        <wssp:SecurityToken TokenType="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#UsernameToken">
          <wssp:UsePassword Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-1.0#PasswordText"/>
        </wssp:SecurityToken>
      </wssp:SupportedTokens>
    </wssp:Identity>
  </wsp:Policy>
  <wsdl:types>
    <xsd:schema
        targetNamespace="http://eg.schema.org/v1"
        xmlns:xsd="http://www.w3.org/2001/XMLSchema"
        >
      <xsd:include schemaLocation="t/eg.xsd" />
    </xsd:schema>
  </wsdl:types>
  <wsdl:message name="firstActionRequest">
    <wsdl:part element="eg:el4" name="firstActionRequestParam"/>
  </wsdl:message>
  <wsdl:message name="firstActionResponse">
    <wsdl:part element="eg:el2" name="firstActionResponseParam"/>
  </wsdl:message>
  <wsdl:portType name="EgService">
    <wsdl:operation name="firstAction" style="document">
      <wsdl:input message="eg:firstActionRequest"/>
      <wsdl:output message="eg:firstActionResponse"/>
    </wsdl:operation>
  </wsdl:portType>
  <wsdl:binding name="EgServiceBinding" type="eg:EgService">
    <wsdlsoap:binding style="document" transport="http://schemas.xmlsoap.org/soap/http"/>
    <wsdl:operation name="firstAction">
      <wsdlsoap:operation soapAction="http://localhost:3030/firstAction"/>
      <wsdl:input>
        <wsp:Policy>
          <wsp:PolicyReference URI="#Auth.xml"/>
        </wsp:Policy>
        <wsdlsoap:body use="literal"/>
      </wsdl:input>
      <wsdl:output>
        <wsdlsoap:body use="literal"/>
      </wsdl:output>
    </wsdl:operation>
  </wsdl:binding>
  <wsdl:service name="EgService">
    <wsdl:port binding="eg:EgServiceBinding" name="EgServicePort">
      <wsdlsoap:address location="http://localhost:3030/"/>
    </wsdl:port>
  </wsdl:service>
</wsdl:definitions>
EOF
    ),
);

ok $doc, "Got a parser object";
ok scalar( @{ $doc->messages }      ), "Got some messages";
ok scalar( @{ $doc->schemas }  ), "Got some schemas";

done_testing();
exit;

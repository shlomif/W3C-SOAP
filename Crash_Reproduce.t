#!/usr/bin/perl

use strict;
use warnings;

use Carp ();

package My::W3C::SOAP::Utils;

sub split_ns {
    my ($tag) = @_;
    Carp::confess "No XML tag passed to split!\n" unless defined $tag;
    my ($ns, $name) = split /:/, $tag, 2;
    return $name ? ($ns, $name) : ('', $ns);
}

package My::W3C::SOAP::Document;

# Created on: 2012-05-27 19:26:43
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use TryCatch;
use URI;
use XML::LibXML;

has string => (
    is         => 'rw',
    isa        => 'Str',
);
has location => (
    is         => 'rw',
    isa        => 'Str',
);
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
has target_namespace => (
    is         => 'rw',
    isa        => 'Str',
    builder    => '_target_namespace',
    lazy_build => 1,
);
has ns_module_map => (
    is        => 'rw',
    isa       => 'HashRef[Str]',
    required  => 1,
);
has module_base => (
    is        => 'rw',
    isa       => 'Str',
    predicate => 'has_module_base',
);

around BUILDARGS => sub {
    my ($orig, $class, @args) = @_;
    my $args
        = !@args     ? {}
        : @args == 1 ? $args[0]
        :              {@args};

    delete $args->{module_base} if ! defined $args->{module_base};

    if ( $args->{string} ) {
        try {
            $args->{xml} = XML::LibXML->load_xml(string => $args->{string});
        }
        catch($e) {
            chomp $e;
            die "Foo";
        }
    }
    elsif ( $args->{location} ) {
        try {
            $args->{xml} = XML::LibXML->load_xml(location => $args->{location});
        }
        catch($e) {
            chomp $e;
            die "Bar";
        }
    }

    return $class->$orig($args);
};

sub _xpc {
    my ($self) = @_;
    my $xpc = XML::LibXML::XPathContext->new($self->xml);
    $xpc->registerNs(xs   => 'http://www.w3.org/2001/XMLSchema');
    $xpc->registerNs(xsd  => 'http://www.w3.org/2001/XMLSchema');
    $xpc->registerNs(wsdl => 'http://schemas.xmlsoap.org/wsdl/');
    $xpc->registerNs(soap => 'http://schemas.xmlsoap.org/wsdl/soap/');

    return $xpc;
}

my $anon = 0;
sub _target_namespace {
    my ($self) = @_;
    my $ns  = $self->xml->getDocumentElement->getAttribute('targetNamespace');
    my $xpc = $self->xpc;
    $xpc->registerNs(ns => $ns) if $ns;

    $ns ||= $self->location || 'NsAnon' . $anon++;

    return $ns;
}

package My::W3C::SOAP::Document::Node;

# Created on: 2012-05-26 19:04:19
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use Carp;


has node => (
    is       => 'rw',
    isa      => 'XML::LibXML::Node',
    required => 1,
);
has parent_node => (
    is        => 'rw',
    isa       => 'Maybe[My::W3C::SOAP::Document::Node]',
    predicate => 'has_parent_node',
    weak_ref  => 1,
);
has document => (
    is         => 'rw',
    isa        => 'My::W3C::SOAP::Document',
    required   => 1,
    builder    => '_document',
    lazy_build => 1,
    weak_ref   => 1,
    handles    => {
        xpc => 'xpc',
    },
);
has name => (
    is         => 'rw',
    isa        => 'Maybe[Str]',
    predicate  => 'has_name',
    builder    => '_name',
    lazy_build => 1,
);

around BUILDARGS => sub {
    my ($orig, $class, @args) = @_;
    my $args
        = !@args     ? {}
        : @args == 1 ? $args[0]
        :              {@args};

    Carp::confess "If document is not specified parent_node must be defined!\n"
        if !$args->{document} && !$args->{parent_node};

    return $class->$orig($args);
};

sub _document {
    my ($self) = shift;
    Carp::confess "Lazybuild $self has both no parent_node nore document!\n" if !$self->has_parent_node || !defined $self->parent_node;
    return $self->parent_node->isa('My::W3C::SOAP::Document') ? $self->parent_node : $self->parent_node->document;
}

sub _name {
    my ($self) = shift;
    return $self->node->getAttribute('name');
}

sub perl_name {
    my ($self) = @_;
    my $name = $self->name;
    return if !$name;

    $name =~ s/ (?<= [^A-Z_] ) ([A-Z]) /_$1/gxms;
    return lc $name;
}

package My::W3C::SOAP::XSD::Document::Node;

# Created on: 2012-05-26 19:04:19
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;

extends 'My::W3C::SOAP::Document::Node';


has '+parent_node' => (
    isa    => 'Maybe[My::W3C::SOAP::XSD::Document::Node]',
);

package My::W3C::SOAP::XSD::Document::Element;

# Created on: 2012-05-26 19:04:09
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use Carp;

extends 'My::W3C::SOAP::XSD::Document::Node';


has complex_type => (
    is     => 'rw',
    isa    => 'Str',
    builder => '_complex_type',
    lazy_build => 1,
);
has type => (
    is     => 'rw',
    isa    => 'Str',
    builder => '_type',
    lazy_build => 1,
    predicate  => 'has_type',
);
has max_occurs => (
    is     => 'rw',
    #isa    => 'Str',
    builder => '_max_occurs',
    lazy_build => 1,
);
has min_occurs => (
    is     => 'rw',
    #isa    => 'Str',
    builder => '_min_occurs',
    lazy_build => 1,
);

sub _complex_type {
    my ($self) = @_;
    my $complex;
    my @nodes = $self->document->xpc->findnodes('xsd:complexType', $self->node);

    for my $node (@nodes) {
    }

    return $complex;
}

sub _type {
    my ($self) = @_;
    my $type = $self->node->getAttribute('type');
    return $type if $type;

    my $simple = $self->document->simple_type;
    TYPE:
    for my $type (keys %{$simple}) {
        my $node = $simple->{$type}->node;
        my  $type_name = $node->parentNode->getAttribute('name');
        if ( $type_name && $self->name && $type_name eq $self->name ) {
            my @children = $self->document->xpc->findnodes('xsd:restriction', $node);
            last if @children != 1;

            my $child = $children[0]->firstChild;
            while ($child) {
                last TYPE if $child->nodeName !~ /^#/;
                $child = $child->nextSibling;
            }

            return $children[0]->getAttribute('base');
        }
        $type_name ||= '';
    }

    return $self->has_anonymous;
}

sub _max_occurs {
    my ($self) = @_;
    return $self->node->getAttribute('maxOccurs') || 1;
}

sub _min_occurs {
    my ($self) = @_;
    return $self->node->getAttribute('minOccurs') || 0;
}

sub simple_type {
    my ($self) = @_;
    $self->document->simple_type();
    my ($ns, $type) = My::W3C::SOAP::Utils::split_ns($self->type);
    $ns ||= $self->document->ns_name;
    return "xs:$type"
        if $self->document->ns_map->{$ns}
            && $self->document->ns_map->{$ns} eq 'http://www.w3.org/2001/XMLSchema';

    my $ns_uri = $self->document->get_ns_uri($ns, $self->node);
    warn "Simple type missing a type for '".$self->type."'\n"
        if !$ns && $ns_uri ne 'http://www.w3.org/2001/XMLSchema';

    return "xs:$type" if $ns_uri eq 'http://www.w3.org/2001/XMLSchema';

    my @xsds = ($self->document);
    while ( my $xsd = shift @xsds ) {
        my $simple = $xsd->simple_type;
        if ( !$simple && @{ $xsd->simple_types } ) {
            $simple = $xsd->simple_type($xsd->_simple_type);
            #warn $xsd->target_namespace . " $type => $simple\n" if $type eq 'GetCreateUIDResponseDto';
        }

        return $simple->{$type}->moose_type if $simple && $simple->{$type};

        push @xsds, @{$xsd->imports};
    }
    return;
}

sub very_simple_type {
    my ($self) = @_;
    $self->document->simple_type();
    my ($ns, $type) = My::W3C::SOAP::Utils::split_ns($self->type);
    $ns ||= $self->document->ns_name;
    return "xs:$type" if $self->document->ns_map->{$ns} && $self->document->ns_map->{$ns} eq 'http://www.w3.org/2001/XMLSchema';

    my $ns_uri = $self->document->get_ns_uri($ns, $self->node);
    warn "Simple type missing a type for '".$self->type
        if !$ns && $ns_uri ne 'http://www.w3.org/2001/XMLSchema';

    return "xs:$type" if $ns_uri eq 'http://www.w3.org/2001/XMLSchema';

    my @xsds = ($self->document);
    while ( my $xsd = shift @xsds ) {
        my $simple = $xsd->simple_type;
        if ( !$simple && @{ $xsd->simple_types } ) {
            $simple = $xsd->simple_type($xsd->_simple_type);
        }

        return $simple->{$type}->type if $simple && $simple->{$type};

        push @xsds, @{$xsd->imports};
    }
    return;
}

sub has_anonymous {
    my ($self) = @_;
    return if $self->has_type && $self->type;

    my %map = reverse %{ $self->document->ns_map };

    my $simple = $self->document->simple_type;
    for my $type (keys %{$simple}) {
        my  $type_name = $simple->{$type}->node->parentNode->getAttribute('name');
        if ( $type_name && $self->name && $type_name eq $self->name ) {
            return $map{$self->document->target_namespace} . ':' . $type;
        }
        $type_name ||= '';
    }

    my $complex = $self->document->complex_type;
    for my $type (keys %{$complex}) {
        my  $type_name = $complex->{$type}->node->parentNode->getAttribute('name');
        if ( $type_name && $self->name && $type_name eq $self->name ) {
            return $map{$self->document->target_namespace} . ':' . $type;
        }
        $type_name ||= '';
    }

    $self->document->ns_map->{xs} ||= 'http://www.w3.org/2001/XMLSchema';
    return 'xs:string';
}

package My::W3C::SOAP::XSD::Document::SimpleType;

# Created on: 2012-05-26 19:04:19
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use Carp;

extends 'My::W3C::SOAP::XSD::Document::Node';


has type => (
    is         => 'rw',
    isa        => 'Str',
    builder    => '_type',
    lazy_build => 1,
);
has maxLength => (
    is         => 'rw',
    isa        => 'Maybe[Int]',
    builder    => '_minLength',
    lazy_build => 1,
);
has minLength => (
    is         => 'rw',
    isa        => 'Maybe[Int]',
    builder    => '_maxLength',
    lazy_build => 1,
);
has length => (
    is         => 'rw',
    isa        => 'Maybe[Int]',
    builder    => '_length',
    lazy_build => 1,
);

sub _type {
    my ($self) = @_;
    my ($restriction) = $self->document->xpc->findnodes('xsd:restriction', $self->node);

    return $restriction->getAttribute('base');
}

sub _maxLength { return shift->_build_restriction('maxLength') }
sub _minLength { return shift->_build_restriction('minLength') }
sub _length    { return shift->_build_restriction('length')    }
sub _build_restriction {
    my ($self, $type) = @_;
    my ($node) = $self->document->xpc->findnodes("xsd:restriction/xsd:$type", $self->node);
    return $node->getAttribute('value');
}

sub moose_type {
    my ($self) = @_;

    warn "No name for ".$self->node->toString if !$self->name;
    my $type = $self->document->module . ':' . $self->name;

    return $type;
}

package My::W3C::SOAP::XSD::Document::ComplexType;

# Created on: 2012-05-26 19:04:25
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;

extends 'My::W3C::SOAP::XSD::Document::Node';


has sequence => (
    is      => 'rw',
    isa     => 'ArrayRef[My::W3C::SOAP::XSD::Document::Element]',
    builder => '_sequence',
    lazy_build => 1,
);

sub _sequence {
    my ($self) = @_;
    my ($node) = $self->document->xpc->findnodes('xsd:complexContent/xsd:extension', $self->node);
    return $self->_get_sequence_elements($node || $self->node);
}

sub _get_sequence_elements {
    my ($self, $node) = @_;
    my @nodes = $self->document->xpc->findnodes('xsd:sequence/*', $node);
    my @sequence;

    for my $node (@nodes) {
        if ( $node->nodeName =~ /(?:^|:)element$/ ) {
            push @sequence, My::W3C::SOAP::XSD::Document::Element->new(
                parent_node => $self,
                node   => $node,
            );
        }
        elsif ( $node->nodeName =~ /(?:^|:)choice$/ ) {
            my @choices = $self->document->xpc->findnodes('xsd:element', $node);
            for my $choice (@choices) {
                push @sequence, My::W3C::SOAP::XSD::Document::Element->new(
                    parent_node  => $self,
                    node         => $choice,
                );
            }
        }
    }

    return \@sequence;
}

1;

package My::W3C::SOAP::XSD::Document;

# Created on: 2012-05-26 15:46:31
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use Path::Class;
use XML::LibXML;
use TryCatch;
use URI;

extends 'My::W3C::SOAP::Document';


has imports => (
    is         => 'rw',
    isa        => 'ArrayRef[My::W3C::SOAP::XSD::Document]',
    builder    => '_imports',
    lazy_build => 1,
);
has simple_types => (
    is         => 'rw',
    isa        => 'ArrayRef[My::W3C::SOAP::XSD::Document::SimpleType]',
    builder    => '_simple_types',
    lazy_build => 1,
);
has simple_type => (
    is         => 'rw',
    isa        => 'HashRef[My::W3C::SOAP::XSD::Document::SimpleType]',
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
has complex_types => (
    is         => 'rw',
    isa        => 'ArrayRef[My::W3C::SOAP::XSD::Document::ComplexType]',
    builder    => '_complex_types',
    lazy_build => 1,
);
has complex_type => (
    is         => 'rw',
    isa        => 'HashRef[My::W3C::SOAP::XSD::Document::ComplexType]',
    builder    => '_complex_type',
    lazy_build => 0,
);
has anon_complex_type_count => (
    is      => 'ro',
    isa     => 'Int',
    traits  => [qw/Counter/],
    default => -1,
    handles => { complex_type_count => 'inc' },
);
has elements => (
    is         => 'rw',
    isa        => 'ArrayRef[My::W3C::SOAP::XSD::Document::Element]',
    builder   => '_elements',
    lazy_build => 1,
);
has ns_name => (
    is        => 'rw',
    isa       => 'Str',
    builder   => '_ns_name',
    lazy_build => 1,
);
has ns_map => (
    is         => 'rw',
    isa        => 'HashRef[Str]',
    predicate  => 'has_ns_map',
    builder    => '_ns_map',
    lazy_build => 1,
);

sub _imports {
    my ($self) = @_;
    my @imports;
    my @nodes = $self->xpc->findnodes('//xsd:import');

    for my $import (@nodes) {
        next if $import->getAttribute('namespace') && $import->getAttribute('namespace') eq 'http://www.w3.org/2001/XMLSchema';

        my $location = $import->getAttribute('schemaLocation');
        if ($location) {

            if ( $self->location && (
                    $self->location =~ m{^(?:https?|ftp)://}
                    || (
                        -f $self->location
                        && !-f $location
                    )
                )
            ) {
                my $current_location
                    = -f $self->location
                    ? file($self->location)->absolute . ''
                    : $self->location;

                $location = URI->new_abs($location, $current_location)->as_string;
            }

            push @imports, __PACKAGE__->new(
                location      => $location,
                ns_module_map => $self->ns_module_map,
                module_base   => $self->module_base,
            );
        }
    }

    return \@imports;
}

sub _simple_types {
    my ($self) = @_;
    my @simple_types;
    my @nodes = $self->xpc->findnodes('//xsd:simpleType');

    for my $node (@nodes) {
        push @simple_types, My::W3C::SOAP::XSD::Document::SimpleType->new(
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

sub _complex_types {
    my ($self) = @_;
    my @complex_types;
    my @nodes = $self->xpc->findnodes('/*/xsd:complexType');

    for my $node (@nodes) {
        # get all top level complex types
        try {
            push @complex_types, My::W3C::SOAP::XSD::Document::ComplexType->new(
                document => $self,
                node     => $node,
            );
        }
        catch ($e) {
            die $e;
        }

    }

    # now itterate over all document level elements and elements of complex types
    my @elements = ( @{ $self->elements }, map {@{ $_->sequence }} @complex_types );

    while ( my $element = shift @elements ) {
        # Get the elements first sub complex type (if any)
        my ($node) = $self->xpc->findnodes('xsd:complexType', $element->node);
        next unless $node;

        try {
            push @complex_types, My::W3C::SOAP::XSD::Document::ComplexType->new(
                parent_node => $element,
                document    => $self,
                node        => $node,
            );
            push @elements, @{ $complex_types[-1]->sequence };
        }
        catch ($e) {
            die $e;
        }
    }

    return \@complex_types;
}

sub _complex_type {
    my ($self) = @_;
    my %complex_type;
    for my $type (@{ $self->complex_types }) {
        my $name = $type->name;
        if ( !$name ) {
            my $parent = $type->node->parentNode;
            $name = $parent->getAttribute('name');
            $name = $name ? $name . 'Type' : 'Anon'.$self->complex_type_count;
            $type->name($name);
        }
        Carp::confess "No name for complex type ".$type->node->parentNode->toString if !$name;
        $complex_type{$name} = $type;
    }

    return \%complex_type;
}

sub _elements {
    my ($self) = @_;
    my @elements;
    my @nodes = $self->xpc->findnodes('/*/xsd:element');

    for my $node (@nodes) {
        push @elements, My::W3C::SOAP::XSD::Document::Element->new(
            document => $self,
            node   => $node,
        );
    }

    return \@elements;
}

sub _ns_name {
    my ($self) = @_;
    my %rev = reverse %{ $self->ns_map };
    if ( !$rev{$self->target_namespace} ) {
        delete $self->ns_map->{''};
        my $ns = $self->target_namespace;
        $ns =~ s/:/_/g;
        $rev{$self->target_namespace} = $ns;
        $self->ns_map->{$ns} = $self->target_namespace;
    }
    Carp::confess "No ns name\n".$self->target_namespace if !$rev{$self->target_namespace};
    return $rev{$self->target_namespace};
}

sub _ns_map {
    my ($self) = @_;

    my %map
        = map {$_->name =~ /^xmlns:?(.*)$/; ($1 => $_->value)}
        grep { $_->name =~ /^xmlns/ }
        $self->xml->getDocumentElement->getAttributes;

    my %rev;
    for my $name ( keys %map ) {
        $rev{$map{$name}} ||= $name;
    }
    if ( $rev{$self->target_namespace} && $map{''} && $map{''} eq $self->target_namespace ) {
        delete $map{''};
    }

    my $ns = $self->target_namespace;
    $ns =~ s/:/_/g;
    $map{$ns} = $self->target_namespace if !$rev{$self->target_namespace};

    return \%map;
}

sub get_ns_uri {
    my ($self, $ns_name, $node) = @_;
    Carp::confess "No namespace passed when trying to map a namespace uri!\n" if !defined $ns_name;

    return $self->ns_map->{$ns_name} if $self->ns_map->{$ns_name};

    if ( $ns_name =~ /:/ ) {
        my $tmp_ns_name = $ns_name;
        $tmp_ns_name =~ s/:/_/g;
        return $self->ns_map->{$tmp_ns_name} if $self->ns_map->{$tmp_ns_name};
    }

    while ($node) {
        my $ns = $node->getAttribute("xmlns:$ns_name");
        return $ns if $ns;
        $ns = $node->getAttribute("targetNamespace");
        return $ns if $ns;
        $node = $node->parentNode;
        last if ref $node eq 'XML::LibXML::Document';
    }

    Carp::confess "Couldn't find the namespace '$ns_name' to map\nMap has:\n", $self->ns_map if !$self->ns_map->{$ns_name};

    return $self->ns_map->{$ns_name};
}

package My::W3C::SOAP::WSDL::Document::Message;

# Created on: 2012-05-27 19:25:15
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;

extends 'My::W3C::SOAP::Document::Node';

has type => (
    is         => 'rw',
    isa        => 'Maybe[Str]',
    builder    => '_type',
    lazy_build => 1,
);

sub _type {
    my ($self) = @_;
    my ($part) = $self->document->xpc->findnodes("wsdl:part", $self->node);
    return unless $part;
    my $type = $part->getAttribute('type');
    return unless $type;

    return $type;
}

package My::W3C::SOAP::WSDL::Document;

# Created on: 2012-05-27 18:57:29
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use Path::Class;
use XML::LibXML;

extends 'My::W3C::SOAP::Document';


has messages => (
    is         => 'rw',
    isa        => 'ArrayRef[My::W3C::SOAP::WSDL::Document::Message]',
    builder    => '_messages',
    lazy_build => 1,
);
has message => (
    is         => 'rw',
    isa        => 'HashRef[My::W3C::SOAP::WSDL::Document::Message]',
    builder    => '_message',
    lazy_build => 1,
    weak_ref   => 1,
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
        push @messages, My::W3C::SOAP::WSDL::Document::Message->new(
            document => $self,
            node   => $node,
        );
    }

    return \@messages;
}

sub _message {
    my ($self) = @_;
    my %message;
    for my $message ( @{ $self->messages }) {
        $message{$message->name} = $message;
    }

    return \%message;
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

        my @args;
        if ( $self->has_module_base ) {
            my $base = $self->module_base;
            $base =~ s/WSDL/XSD/;
            $base .= '::XSD' if ! $base =~ /XSD/;
            push @args, ( module_base => $base );
        }

        push @schemas, My::W3C::SOAP::XSD::Document->new(
            string        => $node->toString,
            ns_module_map => $self->ns_module_map,
            @args,
        );
        $schemas[-1]->location($self->location);
        $schemas[-1]->target_namespace;
    }

    return \@schemas;
}

package My::W3C::SOAP::WSDL::Parser;

use Moose;

has 'document' => (
    is       => 'rw',
    isa      => 'My::W3C::SOAP::WSDL::Document',
    required => 1,
);
has lib => (
    is        => 'rw',
    isa       => 'Str',
    predicate => 'has_lib',
);
has location => (
    is  => 'rw',
    isa => 'Str',
);

around BUILDARGS => sub {
    my ($orig, $class, @args) = @_;
    my $args
        = !@args     ? {}
        : @args == 1 ? $args[0]
        :              {@args};

    my $type = $class;
    $type =~ s/Parser/Document/;

    for my $arg ( keys %$args ) {
        if ( $arg eq 'location' || $arg eq 'string' ) {
            $args->{document} = $type->new($args);
        }
    }

    return $class->$orig($args);
};

package main;

use Test::More;

# create the parser object
my $parser = My::W3C::SOAP::WSDL::Parser->new(
    location      => 't/eg.wsdl',
    lib           => './t/lib',
    ns_module_map => {
        'http://eg.schema.org/v1'     => 'MyApp::Eg',
        'http://parent.schema.org/v1' => 'MyApp::Parent',
        'http://other.schema.org/v1/'  => 'MyApp::Other',
    },
);

ok $parser, "Got a parser object";
is $parser->document->target_namespace, 'http://eg.schema.org/v1', "Get target namespace";
ok scalar( @{ $parser->document->messages }      ), "Got some messages";
ok scalar( @{ $parser->document->schemas }  ), "Got some schemas";

done_testing();
exit;

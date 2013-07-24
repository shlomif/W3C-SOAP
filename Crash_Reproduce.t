#!/usr/bin/perl

use strict;
use warnings;

use Test::More;
use File::ShareDir qw/dist_dir/;
use Template;

package My::W3C::SOAP::Utils;

use URI;
use Carp ();

sub cmp_ns {
    my ($ns1, $ns2) = @_;

    return normalise_ns($ns1) eq normalise_ns($ns2);
}

sub xml_error {
    my ($node) = @_;
    my @lines  = split /\r?\n/, $node->toString;
    my $indent = '';
    if ( $lines[0] !~ /^\s+/ && $lines[-1] =~ /^(\s+)/ ) {
        $indent = $1;
    }
    my $error = $indent . $node->toString."\n at ";
    $error .= "line - ".$node->line_number.' ' if $node->line_number;
    $error .= "path - ".$node->nodePath;

    return $error;
}

sub normalise_ns {
    my ($ns) = @_;

    my $uri = URI->new($ns);

    if ( $uri->can('host') ) {
        $uri->host(lc $uri->host);
    }

    return "$uri";
}

sub split_ns {
    my ($tag) = @_;
    Carp::confess "No XML tag passed to split!\n" unless defined $tag;
    my ($ns, $name) = split /:/, $tag, 2;
    return $name ? ($ns, $name) : ('', $ns);
}

sub ns2module {
    my ($ns) = @_;

    my $uri = URI->new($ns);

    # URI's which have a host an a path are converted Java style name spacing
    if ( $uri->can('host') && $uri->can('path') ) {
        my $module = join '::', reverse map {ucfirst $_} map {lc $_} map {s/\W/_/g; $_} split /[.]/, $uri->host; ## no critic
        $module .= join '::', map {s/\W/_/g; $_} split m{/}, $uri->path; ## no critic
        return $module;
    }

    # other URI's are just made safe as a perl module name.
    $ns =~ s{://}{::};
    $ns =~ s{([^:]:)([^:])}{$1:$2}g;
    $ns =~ s{[^\w:]+}{_}g;

    return $ns;
}

package My::W3C::SOAP::Document;

# Created on: 2012-05-27 19:26:43
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use Carp qw/croak cluck confess longmess/;
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
    clearer    => 'clear_xpc',
    predicate  => 'has_xpc',
    lazy_build => 1,
);
has target_namespace => (
    is         => 'rw',
    isa        => 'Str',
    builder    => '_target_namespace',
    predicate  => 'has_target_namespace',
    lazy_build => 1,
);
has ns_module_map => (
    is        => 'rw',
    isa       => 'HashRef[Str]',
    required  => 1,
    predicate => 'has_ns_module_map',
);
has module => (
    is        => 'rw',
    isa       => 'Str',
    predicate => 'has_module',
    builder   => '_module',
    lazy_build => 1,
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

sub _module {
    my ($self) = @_;
    my $ns = $self->target_namespace;

    if ( $self->has_module_base ) {
        $self->ns_module_map->{My::W3C::Utils::normalise_ns($self->target_namespace)}
            = $self->module_base . '::' . My::W3C::Utils::ns2module($self->target_namespace);
    }

    if ( !$self->ns_module_map->{My::W3C::Utils::normalise_ns($ns)} && $self->ns_module_map->{$ns} ) {
        $self->ns_module_map->{My::W3C::Utils::normalise_ns($ns)} = $self->ns_module_map->{$ns};
    }

    confess "Trying to get module mappings when none specified!\n" if !$self->has_ns_module_map;
    confess "No mapping specified for the namespace ", $ns, "!\n"  if !$self->ns_module_map->{My::W3C::Utils::normalise_ns($ns)};

    return $self->ns_module_map->{My::W3C::Utils::normalise_ns($ns)};
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

    confess "If document is not specified parent_node must be defined!\n"
        if !$args->{document} && !$args->{parent_node};

    return $class->$orig($args);
};

sub _document {
    my ($self) = shift;
    confess "Lazybuild $self has both no parent_node nore document!\n" if !$self->has_parent_node || !defined $self->parent_node;
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

package My::W3C::SOAP::XSD::Document::Type;

# Created on: 2012-06-06 14:00:31
# Create by:  dev
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use Carp;

extends 'My::W3C::SOAP::XSD::Document::Node';


has documentation => (
    is     => 'rw',
    isa    => 'Str',
    builder => '_documentation',
    lazy_build => 1,
);

sub _documentation {
    my ($self) = @_;
    my ($documentation) = $self->document->xpc->findnodes('xsd:annotation/xsd:documentation', $self->node);

    return '' unless $documentation;

    $documentation = $documentation->textContent;
    $documentation =~ s/^\s+|\s+$//g;

    return $documentation;
}

1;

package My::W3C::SOAP::XSD::Document::Element;

# Created on: 2012-05-26 19:04:09
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use Carp;

extends 'My::W3C::SOAP::XSD::Document::Type';


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
has package => (
    is     => 'rw',
    isa    => 'Str',
    builder => '_package',
    lazy_build => 1,
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
has nillable => (
    is     => 'rw',
    isa    => 'Bool',
    builder => '_nillable',
    lazy_build => 1,
);
has choice_group => (
    is     => 'rw',
    isa    => 'Int',
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

sub _package {
    my ($self) = @_;
    my $type = $self->type;
    my ($ns, $name) = My::W3C::SOAP::Utils::split_ns($type);
    $ns ||= $self->document->ns_name;
    my $ns_uri = $name ? $self->document->get_ns_uri($ns, $self->node) : '';
    $name ||= $ns;

    if ( $ns_uri eq 'http://www.w3.org/2001/XMLSchema' ) {
        return "xs:$name";
    }

    my $base = $self->document->get_module_base( $ns_uri || $self->document->target_namespace );

    return $base . '::' . $name;
}

sub _max_occurs {
    my ($self) = @_;
    return $self->node->getAttribute('maxOccurs') || 1;
}

sub _min_occurs {
    my ($self) = @_;
    return $self->node->getAttribute('minOccurs') || 0;
}

sub _nillable {
    my ($self) = @_;
    my $nillable = $self->node->getAttribute('nillable');

    return !$nillable          ? 1
        : $nillable eq 'true'  ? 1
        : $nillable eq 'false' ? 0
        :                        die "Unknown value for attribute nillable in ".$self->node->toString;
}

sub module {
    my ($self) = @_;

    return $self->document->module;
}

sub type_module {
    my ($self) = @_;
    my ($ns, $type) = My::W3C::SOAP::Utils::split_ns($self->type);
    $ns ||= $self->document->ns_name;
    my $ns_uri = $self->document->get_ns_uri($ns, $self->node);

    return $self->simple_type || $self->document->get_module_base( $ns_uri ) . '::' . $type;
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
    warn "Simple type missing a type for '".$self->type."'\n".My::W3C::Utils::xml_error($self->node)."\n"
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
    warn "Simple type missing a type for '".$self->type."'\n".My::W3C::Utils::xml_error($self->node)."\n"
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

sub moosex_type {
    my ($self) = @_;
    my ($ns, $type) = My::W3C::SOAP::Utils::split_ns($self->type);
    $ns ||= $self->document->ns_name;
    my $ns_uri = $self->document->get_ns_uri($ns, $self->node);
    warn "Simple type missing a type for '".$self->type."'\n".My::W3C::Utils::xml_error($self->node)."\n"
        if !$ns && $ns_uri ne 'http://www.w3.org/2001/XMLSchema';

    return "'xs:$type'" if $ns_uri eq 'http://www.w3.org/2001/XMLSchema';

    my @xsds = ($self->document);
    while ( my $xsd = shift @xsds ) {
        my $simple = $xsd->simple_type;
        if ( !$simple && @{ $xsd->simple_types } ) {
            $simple = $xsd->simple_type($xsd->_simple_type);
            #warn $xsd->target_namespace . " $type => $simple\n" if $type eq 'GetCreateUIDResponseDto';
        }

        return $simple->{$type}->moosex_type if $simple && $simple->{$type};

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

extends 'My::W3C::SOAP::XSD::Document::Type';


has type => (
    is         => 'rw',
    isa        => 'Str',
    builder    => '_type',
    lazy_build => 1,
);
has enumeration => (
    is         => 'rw',
    isa        => 'ArrayRef[Str]',
    builder    => '_enumeration',
    lazy_build => 1,
);
has maxLength => (
    is         => 'rw',
    isa        => 'Maybe[Int]',
    builder    => '_minLength',
    predicate  => 'has_minLength',
    lazy_build => 1,
);
has minLength => (
    is         => 'rw',
    isa        => 'Maybe[Int]',
    builder    => '_maxLength',
    predicate  => 'has_maxLength',
    lazy_build => 1,
);
has length => (
    is         => 'rw',
    isa        => 'Maybe[Int]',
    builder    => '_length',
    predicate  => 'has_length',
    lazy_build => 1,
);

sub _type {
    my ($self) = @_;
    my ($restriction) = $self->document->xpc->findnodes('xsd:restriction', $self->node);

    return $restriction->getAttribute('base');
}

sub _enumeration {
    my ($self) = @_;
    my @nodes = $self->document->xpc->findnodes('xsd:restriction/xsd:enumeration', $self->node);
    my @enumeration;

    for my $node (@nodes) {
        push @enumeration, $node->getAttribute('value');
    }

    return \@enumeration;
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

sub moosex_type {
    my ($self) = @_;

    warn "No name for ".$self->node->toString if !$self->name;
    return $self->name;
}

package My::W3C::SOAP::XSD::Document::ComplexType;

# Created on: 2012-05-26 19:04:25
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use Carp;

extends 'My::W3C::SOAP::XSD::Document::Type';


has sequence => (
    is      => 'rw',
    isa     => 'ArrayRef[My::W3C::SOAP::XSD::Document::Element]',
    builder => '_sequence',
    lazy_build => 1,
);
has module => (
    is        => 'rw',
    isa       => 'Str',
    builder   => '_module',
    lazy_build => 1,
);
has extension => (
    is        => 'rw',
    isa       => 'Maybe[Str]',
    builder   => '_extension',
    lazy_build => 1,
);

sub _sequence {
    my ($self) = @_;
    my ($node) = $self->document->xpc->findnodes('xsd:complexContent/xsd:extension', $self->node);
    return $self->_get_sequence_elements($node || $self->node);
}

sub _module {
    my ($self) = @_;

    return $self->document->module . '::' . ( $self->name || $self->parent_node->name );
}

sub _extension {
    my ($self) = @_;
    my @nodes = $self->document->xpc->findnodes('xsd:complexContent/xsd:extension', $self->node);

    for my $node (@nodes) {
        my ($ns, $tag) = My::W3C::SOAP::Utils::split_ns($node->getAttribute('base'));
        my $ns_uri = $self->document->get_ns_uri($ns, $self->node);

        return $self->document->get_module_base( $ns_uri ) . "::$tag";
    }

    return;
}

sub _get_sequence_elements {
    my ($self, $node) = @_;
    my @nodes = $self->document->xpc->findnodes('xsd:sequence/*', $node);
    my @sequence;
    my $group = 1;

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
                    choice_group => $group,
                );
            }
            $group++;
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
use Carp qw/croak cluck confess longmess/;
use Path::Class;
use XML::LibXML;
use WWW::Mechanize;
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
has element => (
    is         => 'rw',
    isa        => 'HashRef[My::W3C::SOAP::XSD::Document::Element]',
    builder   => '_element',
    lazy_build => 1,
);
has module => (
    is        => 'rw',
    isa       => 'Str',
    builder   => '_module',
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
        confess "No name for simple type ".$type->node->parentNode->toString if !$name;
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
        confess "No name for complex type ".$type->node->parentNode->toString if !$name;
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

sub _element {
    my ($self) = @_;
    my %element;
    for my $element (@{ $self->elements }) {
        $element{$element->name} = $element;
    }
    return \%element;
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
    confess "No ns name\n".$self->target_namespace if !$rev{$self->target_namespace};
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
    confess "No namespace passed when trying to map a namespace uri!\n" if !defined $ns_name;

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

    confess "Couldn't find the namespace '$ns_name' to map\nMap has:\n", $self->ns_map if !$self->ns_map->{$ns_name};

    return $self->ns_map->{$ns_name};
}

sub get_module_base {
    my ($self, $ns) = @_;

    confess "Trying to get module mappings when none specified!\n" if !$self->has_ns_module_map;
    confess "No mapping specified for the namespace $ns!\n"        if !$self->ns_module_map->{My::W3C::Utils::normalise_ns($ns)};

    return $self->ns_module_map->{My::W3C::Utils::normalise_ns($ns)};
}

1;
package My::W3C::SOAP::Parser;

# Created on: 2012-05-27 18:58:29
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;

has document => (
    is       => 'rw',
    isa      => 'My::W3C::SOAP::Document',
);
has lib => (
    is        => 'rw',
    isa       => 'Str',
    predicate => 'has_lib',
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

package My::W3C::SOAP::XSD::Types;

# Created on: 2012-05-26 23:08:42
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use strict;
use Carp;
use DateTime::Format::Strptime;
use MooseX::Types -declare
    => [qw/
        xsd:duration
        xsd:dateTime
        xsd:time
        xsd:date
        xsd:gYearMonth
        xsd:gYear
        xsd:gMonthDay
        xsd:gDay
        xsd:gMonth
    /];
use DateTime;
use DateTime::Format::Strptime qw/strptime/;
use Math::BigFloat;


my $sig_warn = $SIG{__WARN__};
$SIG{__WARN__} = sub {};

class_type 'DateTime';
class_type 'XML::LibXML::Node';

subtype 'xsd:boolean',
    as 'xs:boolean';
coerce 'xsd:boolean',
    from 'Str'
        => via {
              $_ eq 'true'  ? 1
            : $_ eq 'false' ? undef
            :                 confess "'$_' isn't a xs:boolean!";
        };

subtype 'xsd:double',
    as 'xs:double';
coerce 'xsd:double',
#    from 'Num'
#        => via { Params::Coerce::coerce('xs:double', $_) },
    from 'Str'
        => via { Math::BigFloat->new($_) };

subtype 'xsd:decimal',
    as 'xs:decimal';
coerce 'xsd:decimal',
#    from 'Num'
#        => via { Params::Coerce::coerce('xs:decimal', $_) },
    from 'Str'
        => via { Math::BigFloat->new($_) };

subtype 'xsd:long',
    as 'xs:long';
coerce 'xsd:long',
#    from 'Num'
#        => via { Params::Coerce::coerce('xs:long', $_) },
    from 'Str'
        => via { Math::BigInt->new($_) };

#subtype 'xsd:duration',
#    as 'DateTime';
#coerce 'xsd:duration',
#    from 'Str',
#    via {
#        DateTime::Format::Strptime("", $_)
#    };
#
subtype 'xsd:dateTime',
    as 'DateTime';
coerce 'xsd:dateTime',
    from 'XML::LibXML::Node' =>
        => via { $_->textContent },
    from 'Str',
        => via {
            return strptime("%FT%T", $_) if /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}$/xms;
            # DateTime expects timezones as [+-]hhmm XMLSchema expects them as [+-]hh:mm
            # also remove any milli seconds
            s/(?:[.]\d+)? ([+-]\d{2}) : (\d{2}) $/$1$2/xms;
            # Dates with timezones are meant to track the begging of the day
            return strptime("%FT%T%z", $_);
        };

#subtype 'xsd:time',
#    as 'DateTime';
#coerce 'xsd:time',
#    from 'Str',
#    via {
#        DateTime::Format::Striptime("", $_)
#    };

subtype 'xsd:date',
    as 'DateTime';
coerce 'xsd:date',
    from 'XML::LibXML::Node' =>
        => via { $_->textContent },
    from 'Str',
        => via {
            return strptime("%F", $_) if /^\d{4}-\d{2}-\d{2}$/xms;
            # DateTime expects timezones as [+-]hhmm XMLSchema expects them as [+-]hh:mm
            s/([+-]\d{2}):(\d{2})$/$1$2/;
            # Dates with timezones are meant to track the begging of the day
            return strptime("%TT%F%z", "00:00:00T$_");
        };

$SIG{__WARN__} = $sig_warn;

1;
package My::W3C::SOAP::XSD;

# Created on: 2012-05-26 23:50:44
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use Carp qw/croak cluck confess longmess/;
use Moose::Util::TypeConstraints;
use MooseX::Types::XMLSchema;
My::W3C::SOAP::XSD::Types->import(':all');
use TryCatch;
use DateTime::Format::Strptime qw/strptime/;


has xsd_ns => (
    is  => 'rw',
    isa => 'Str',
);
has xsd_ns_name => (
    is         => 'rw',
    isa        => 'Str',
    predicate  => 'has_xsd_ns_name',
    clearer    => 'clear_xsd_ns_name',
    builder    => '_xsd_ns_name',
    lazy_build => 1,
);

{
    my %required;
    my $require = sub {
        my ($module) = @_;
        return if $required{$module}++;
        return if UNIVERSAL::can($module, 'new');

        my $file = "$module.pm";
        $file =~ s{::}{/}g;
        require $file;
    };
    around BUILDARGS => sub {
        my ($orig, $class, @args) = @_;
        my $args
            = !@args     ? {}
            : @args == 1 ? $args[0]
            :              {@args};

        if ( blessed $args && $args->isa('XML::LibXML::Node') ) {
            my $xml   = $args;
            my $child = $xml->firstChild;
            my $map   = $class->xml2perl_map;
            my ($element)  = $class =~ /::([^:]+)$/;
            $args = {};

            while ($child) {
                if ( $child->nodeName !~ /^#/ ) {
                    my ($node_ns, $node) = My::W3C::Utils::split_ns($child->nodeName);
                    confess "Could not get node from (".$child->nodeName." via '$node_ns', '$node')\n"
                        if !$map->{$node};
                    my $attrib = $map->{$node};
                    $node = $attrib->name;
                    my $module = $attrib->has_xs_perl_module ? $attrib->xs_perl_module : undef;
                    $require->($module) if $module;
                    my $value  = $module ? $module->new($child) : $child->textContent;

                    $args->{$node}
                        = !exists $args->{$node}        ? $value
                        : ref $args->{$node} ne 'ARRAY' ? [   $args->{$node} , $value ]
                        :                                 [ @{$args->{$node}}, $value ];
                }
                $child = $child->nextSibling;
            }
        }

        return $class->$orig($args);
    };
}

my %ns_map;
my $count = 0;
sub _xsd_ns_name {
    my ($self) = @_;
    my $ns = $self->xsd_ns;

    return $ns_map{$ns} if $ns_map{$ns};

    return $ns_map{$ns} = 'WSX' . $count++;
}

sub xml2perl_map {
    my ($class) = @_;
    my %map;

    for my $attr ($class->get_xml_nodes) {
        $map{$attr->xs_name} = $attr;
    }

    # get super class nodes (if any)
    my $meta = $class->meta;

    for my $super ( $meta->superclasses ) {
        next if !$super->can('xml2perl_map') && $super ne __PACKAGE__;
        %map = ( %{ $super->xml2perl_map }, %map );
    }

    return \%map;
}

# recursively try to find the default value for an attribute
sub _get_attribute_default {
    my ($class, $attribute) = @_;
    my $meta = $class->meta;
    my $attrib = $meta->get_attribute($attribute);

    return $attrib->default if $attrib;

    for my $super ( $meta->superclasses ) {
        my $default = $super->_get_attribute_default($attribute);
        return $default if $default;
    }

    return;
}

my %types;
sub xsd_subtype {
    my ($self, %args) = @_;
    my $parent_type = $args{module} || $args{parent};
    # upgrade dates
    $parent_type
        = $parent_type eq 'xs:date'     ? 'xsd:date'
        : $parent_type eq 'xs:dateTime' ? 'xsd:dateTime'
        : $parent_type eq 'xs:boolean'  ? 'xsd:boolean'
        : $parent_type eq 'xs:double'   ? 'xsd:double'
        : $parent_type eq 'xs:decimal'  ? 'xsd:decimal'
        : $parent_type eq 'xs:long'     ? 'xsd:long'
        :                                 $parent_type;

    my $parent_type_name = $args{list} ? "ArrayRef[$parent_type]" : $parent_type;
    my $subtype = $parent_type =~ /^xsd:\w/ && Moose::Util::TypeConstraints::find_type_constraint($parent_type_name);
    return $subtype if $subtype;

    $subtype = subtype
        as $parent_type_name,
        message {"'$_' failed to validate as a $parent_type"};

    if ( $args{list} ) {
        if ( $args{module} ) {
            coerce $subtype =>
                from 'xml_node' =>
                via { [$parent_type->new($_)] };
            coerce $subtype =>
                from 'HashRef' =>
                via { [$parent_type->new($_)] };
            coerce $subtype =>
                from 'ArrayRef[HashRef]' =>
                via { [ map { $parent_type->new($_) } @$_ ] };
            coerce $subtype =>
                from $parent_type =>
                via { [$_] };
        }
        else {
            coerce $subtype =>
                from 'xml_node' =>
                via { [$_->textContent] };
            coerce $subtype =>
                from 'ArrayRef[xml_node]' =>
                via { [ map { $_->textContent } @$_ ] };
        }
    }
    elsif ( $args{module} ) {
        coerce $subtype =>
            from 'xml_node' =>
            via { $parent_type->new($_) };
        coerce $subtype =>
            from 'HashRef' =>
            via { $parent_type->new($_) };
    }
    else {
        coerce $subtype =>
            from 'xml_node' =>
            via { $_->textContent };
    }

    # Propogate coercion from Any via parent's type coercion.
    my $this_type = $subtype->parent;
    if ($this_type->has_parent) {
        coerce $subtype
            => from 'Any'
            => via { $this_type->parent->coerce($_) };
    }

    return $subtype;
}

package My::W3C::SOAP::XSD::Parser;

# Created on: 2012-05-28 08:11:37
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use List::MoreUtils qw/all/;
use Path::Class;
use File::ShareDir qw/dist_dir/;
use Moose::Util::TypeConstraints;

extends 'My::W3C::SOAP::Parser';


subtype xsd_documents =>
    as 'ArrayRef[My::W3C::SOAP::XSD::Document]';
has '+document' => (
    isa      => 'xsd_documents',
    coerce   => 1,
);
has ns_module_map => (
    is       => 'rw',
    isa      => 'HashRef[Str]',
    required => 1,
);

sub get_schemas {
    my ($self) = @_;
    my @xsds   = @{ $self->document };
    my %xsd;

    # import all schemas
    while ( my $xsd = shift @xsds ) {
        my $target_namespace = $xsd->target_namespace;
        push @{ $xsd{$target_namespace} }, $xsd;

        for my $import ( @{ $xsd->imports } ) {
            push @xsds, $import;
        }
    }

    # flatten schemas specified more than once
    for my $ns ( keys %xsd ) {
        my $xsd = pop @{ $xsd{$ns} };
        if ( @{ $xsd{$ns} } ) {
            for my $xsd_repeat ( @{ $xsd{$ns} } ) {
                push @{ $xsd->simple_types  }, @{ $xsd_repeat->simple_types  };
                push @{ $xsd->complex_types }, @{ $xsd_repeat->complex_types };
                push @{ $xsd->elements      }, @{ $xsd_repeat->elements      };
            }
        }

        push @xsds, $xsd;
    }

    return @xsds;
}

sub dynamic_classes {
    my ($self) = @_;
    my @xsds   = $self->get_schemas;
    my @packages;

    # construct the in memory module names
    for my $xsd (@xsds) {
        $xsd->module_base('Dynamic::XSD');
        $xsd->module;
    }

    my %seen;
    my @ordered_xsds;
    XSD:
    while ( my $xsd = shift @xsds ) {
        my $module = $xsd->module;

        # Complex types
        my @types = @{ $xsd->complex_types };
        my %local_seen;
        TYPE:
        while ( my $type = shift @types ) {
            my $type_name = $type->name || $type->parent_node->name;
            my $type_module = $module . '::' . $type_name;

            if ( $type->extension && !$seen{ $type->extension }++ ) {
                push @xsds, $xsd;
                next XSD;
            }
            $local_seen{ $type_module }++;
        }

        %seen = ( %seen, %local_seen );
        push @ordered_xsds, $xsd;
    }

    my %complex_seen = ( 'My::W3C::SOAP::XSD' => 1 );
    for my $xsd (@ordered_xsds) {
        my $module = $xsd->module;

        # Complex types
        my @complex_types = @{ $xsd->complex_types };
        while ( my $type = shift @complex_types ) {
            my $type_name = $type->name || $type->parent_node->name;
            my $type_module = $module . '::' . $type_name;

            my %modules = ( 'My::W3C::SOAP::XSD' => 1 );
            for my $el (@{ $type->sequence }) {
                $modules{ $el->type_module }++
                    if ! $el->simple_type && $el->module ne $module
            }
            if ( $type->extension ) {
                $modules{ $type->extension }++
            }

            if ( !all {$complex_seen{$_}} keys %modules ) {
                push @complex_types, $type;
                next;
            }

            $complex_seen{$type_module}++;
        }

        push @packages, $module;
    }

    return @packages;
}

sub element_attributes {
    my ($self, $class, $class_name, $element) = @_;

    my $simple = $element->simple_type;
    my $very_simple = $element->very_simple_type;
    my $is_array = $element->max_occurs eq 'unbounded'
        || ( $element->max_occurs && $element->max_occurs > 1 )
        || ( $element->min_occurs && $element->min_occurs > 1 );
    my $type_name = $simple || $element->type_module;
    my $serialize = '';

    if ( $very_simple ) {
        if ( $very_simple eq 'xs:boolean' ) {
            $serialize = sub { $_ ? 'true' : 'false' };
        }
        elsif ( $very_simple eq 'xs:date' ) {
            $serialize = sub {
                return $_->ymd if $_->time_zone->isa('DateTime::TimeZone::Floating');
                my $d = DateTime::Format::Strptime::strftime('%F%z', $_);
                $d =~ s/([+-]\d\d)(\d\d)$/$1:$2/;
                return $d
            };
        }
        elsif ( $very_simple eq 'xs:time' ) {
            $serialize = sub { $_->hms };
        }
    }

    my @extra;
    push @extra, ( xs_perl_module  => $element->type_module  ) if !$simple;
    push @extra, ( xs_choice_group => $element->choice_group ) if $element->choice_group;
    push @extra, ( xs_serialize    => $serialize             ) if $serialize;

    confess "No perl name!\n".$element->node->parentNode->toString if !$element->perl_name;
    $class->add_attribute(
        $element->perl_name,
        is            => 'rw',
        isa           => $class_name->xsd_subtype(
            ($simple ? 'parent' : 'module') => $type_name,
           list => $is_array,
        ),
        predicate     => 'has_'. $element->perl_name,
        # TODO handle nillable correctly  should be a Maybe type
        #required      => !$element->nillable,
        coerce        => 1,
    #[%- IF config->alias && element->name.replace('^\w+:', '') != element->perl_name %]
        #alias         => '[% element->name.replace('^\w+:', '') %]',
    #[%- END %]
        traits        => [qw{ My::W3C::SOAP::XSD }],
        xs_name       => $element->name,
        xs_type       => $element->type,
        xs_min_occurs => $element->min_occurs,
        xs_max_occurs => $element->max_occurs  eq 'unbounded' ? 0 : $element->max_occurs,
        @extra,
    );

    return;
}

package My::W3C::SOAP::WSDL::Document::InOutPuts;

# Created on: 2012-05-28 07:30:02
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use Carp;
extends 'My::W3C::SOAP::Document::Node';


has message => (
    is         => 'rw',
    isa        => 'Maybe[My::W3C::SOAP::WSDL::Document::Message]',
    builder    => '_message',
    lazy_build => 1,
);
has body => (
    is         => 'rw',
    isa        => 'Maybe[Str]',
    builder    => '_body',
    lazy_build => 1,
);

sub _message {
    my ($self) = @_;
    my ($ns, $message) = My::W3C::Utils::split_ns($self->node->getAttribute('message'));

    for my $msg (@{ $self->document->messages }) {
        return $msg if $msg->name eq $message;
    }
}

package My::W3C::SOAP::WSDL::Document::Operation;

# Created on: 2012-05-28 07:03:06
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use Carp;

extends 'My::W3C::SOAP::Document::Node';


has action => (
    is         => 'rw',
    isa        => 'Str',
    builder    => '_action',
    lazy_build => 1,
);
has inputs => (
    is         => 'rw',
    isa        => 'ArrayRef[My::W3C::SOAP::WSDL::Document::InOutPuts]',
    builder    => '_inputs',
    lazy_build => 1,
);
has outputs => (
    is         => 'rw',
    isa        => 'ArrayRef[My::W3C::SOAP::WSDL::Document::InOutPuts]',
    builder    => '_outputs',
    lazy_build => 1,
);

sub _action {
    my ($self) = @_;
    my $action = $self->node->getAttribute('soapAction');
    return $action if $action;
    my ($child) = $self->document->xpc->findnode('soap:binding'. $self->node);
    return $child->getAttribute('soapAction');
}

sub _inputs  { return $_[0]->_in_out_puts('input');  }
sub _outputs { return $_[0]->_in_out_puts('output'); }
sub _in_out_puts {
    my ($self, $dir) = @_;
    my @puts;
    my @nodes = $self->document->xpc->findnodes("wsdl:$dir", $self->node);

    for my $node (@nodes) {
        push @puts, My::W3C::SOAP::WSDL::Document::InOutPuts->new(
            parent_node => $self,
            node        => $node,
        );
    }

    return \@puts;
}

package My::W3C::SOAP::WSDL::Document::Message;

# Created on: 2012-05-27 19:25:15
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use Carp;

extends 'My::W3C::SOAP::Document::Node';


has element => (
    is         => 'rw',
    isa        => 'Maybe[My::W3C::SOAP::XSD::Document::Element]',
    builder    => '_element',
    lazy_build => 1,
);
has type => (
    is         => 'rw',
    isa        => 'Maybe[Str]',
    builder    => '_type',
    lazy_build => 1,
);

sub _element {
    my ($self) = @_;
    my ($part) = $self->document->xpc->findnodes("wsdl:part", $self->node);
    return unless $part;
    my $element = $part->getAttribute('element');
    return unless $element;

    my ($ns, $el_name) = My::W3C::Utils::split_ns($element);
    my $nsuri = $self->document->get_nsuri($ns);
    my @schemas = @{ $self->document->schemas };

    for my $schema (@schemas) {
        push @schemas, @{ $schema->imports };

        if ( My::W3C::Utils::cmp_ns($schema->target_namespace, $nsuri) ) {
            for my $element (@{ $schema->elements }) {
                return $element if $element->name eq $el_name;
            }
        }
    }

    return;
}

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
use Carp;
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
has schema => (
    is         => 'rw',
    isa        => 'HashRef[My::W3C::SOAP::XSD::Document]',
    builder    => '_schema',
    lazy_build => 1,
    weak_ref   => 1,
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

sub _schema {
    my ($self) = @_;
    my %schema;
    for my $schema ( @{ $self->schemas }) {
        $schema{$schema->target_namespace} = $schema;
    }

    return \%schema;
}

sub get_nsuri {
    my ($self, $ns) = @_;
    my ($node) = $self->xpc->findnodes("//namespace::*[name()='$ns']");
    return $node->value;
}

package My::W3C::SOAP::WSDL::Parser;

# Created on: 2012-05-27 18:58:29
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use Path::Class;
use File::ShareDir qw/dist_dir/;

extends 'My::W3C::SOAP::Parser';

has '+document' => (
    isa      => 'My::W3C::SOAP::WSDL::Document',
    required => 1,
);

has location => (
    is  => 'rw',
    isa => 'Str',
);

package main;

# create the parser object
my $parser = My::W3C::SOAP::WSDL::Parser->new(
    location      => 't/eg.wsdl',
    module        => 'MyApp::WsdlEg',
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

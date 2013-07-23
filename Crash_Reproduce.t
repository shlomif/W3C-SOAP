#!/usr/bin/perl

use strict;
use warnings;

use Test::More;
use File::ShareDir qw/dist_dir/;
use Template;

package My::W3C::SOAP::Utils;

use URI;
use Carp ();

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

package My::W3C::SOAP::Document::Node;

# Created on: 2012-05-26 19:04:19
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use warnings;
use version;
use Carp;
use Scalar::Util;
use List::Util;
use Data::Dumper qw/Dumper/;
use English qw/ -no_match_vars /;

our $VERSION    = version->new('0.02');
$ENV{W3C_SOAP_NAME_STYLE} ||= 'perl';

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
    isa        => 'W3C::SOAP::Document',
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
    return $self->parent_node->isa('W3C::SOAP::Document') ? $self->parent_node : $self->parent_node->document;
}

sub _name {
    my ($self) = shift;
    return $self->node->getAttribute('name');
}

sub perl_name {
    my ($self) = @_;
    my $name = $self->name;
    return if !$name;

    return $name if $ENV{W3C_SOAP_NAME_STYLE} eq 'original';

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
use warnings;
use version;
use English qw/ -no_match_vars /;

extends 'My::W3C::SOAP::Document::Node';

our $VERSION     = version->new('0.02');

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
use warnings;
use version;
use Carp;
use Scalar::Util;
use List::Util;
#use List::MoreUtils;
use Data::Dumper qw/Dumper/;
use English qw/ -no_match_vars /;

extends 'My::W3C::SOAP::XSD::Document::Node';

our $VERSION     = version->new('0.02');

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
use warnings;
use version;
use Carp;
use Scalar::Util;
use List::Util;
#use List::MoreUtils;
use Data::Dumper qw/Dumper/;
use English qw/ -no_match_vars /;
use W3C::SOAP::Utils qw/xml_error/;

extends 'My::W3C::SOAP::XSD::Document::Type';

our $VERSION     = version->new('0.02');

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
    warn "Simple type missing a type for '".$self->type."'\n".xml_error($self->node)."\n"
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
    warn "Simple type missing a type for '".$self->type."'\n".xml_error($self->node)."\n"
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
    warn "Simple type missing a type for '".$self->type."'\n".xml_error($self->node)."\n"
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
use warnings;
use version;
use Carp;
use Scalar::Util;
use List::Util;
#use List::MoreUtils;
use Data::Dumper qw/Dumper/;
use English qw/ -no_match_vars /;
use W3C::SOAP::Utils qw/split_ns/;

extends 'My::W3C::SOAP::XSD::Document::Type';

our $VERSION     = version->new('0.02');

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
has pattern => (
    is         => 'rw',
    isa        => 'Maybe[Str]',
    builder    => '_pattern',
    predicate  => 'has_pattern',
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

sub _pattern   { return shift->_build_restriction('pattern')   }
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

sub moose_base_type {
    my ($self) = @_;
    my ($ns, $type) = split_ns($self->type);
    $ns ||= $self->document->target_namespace;
    return "xs:$type" if $self->document->ns_map->{$ns} && $self->document->ns_map->{$ns} eq 'http://www.w3.org/2001/XMLSchema';

    my $ns_uri = $self->document->get_ns_uri($ns, $self->node);

    return "xs:$type" if $ns_uri eq 'http://www.w3.org/2001/XMLSchema';

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
use warnings;
use version;
use Carp;
use Scalar::Util;
use List::Util;
#use List::MoreUtils;
use Data::Dumper qw/Dumper/;
use English qw/ -no_match_vars /;

extends 'My::W3C::SOAP::XSD::Document::Type';

our $VERSION     = version->new('0.02');

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
has complex_content => (
    is        => 'rw',
    isa       => 'Str',
    builder   => '_complex_content',
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

sub _complex_content {
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
use version;
use Carp qw/carp croak cluck confess longmess/;
use Scalar::Util;
use List::Util;
#use List::MoreUtils;
use Data::Dumper qw/Dumper/;
use English qw/ -no_match_vars /;
use Path::Class;
use XML::LibXML;
use WWW::Mechanize;
use TryCatch;
use URI;
use W3C::SOAP::Utils qw/normalise_ns/;

extends 'W3C::SOAP::Document';

our $VERSION     = version->new('0.02');

has imports => (
    is         => 'rw',
    isa        => 'ArrayRef[My::W3C::SOAP::XSD::Document]',
    builder    => '_imports',
    lazy_build => 1,
);
has imported => (
    is         => 'rw',
    isa        => 'HashRef[My::W3C::SOAP::XSD::Document]',
    builder    => '_imported',
    lazy_build => 1,
);
has includes => (
    is         => 'rw',
    isa        => 'ArrayRef[My::W3C::SOAP::XSD::Document]',
    builder    => '_includes',
    lazy_build => 1,
);
has include => (
    is         => 'rw',
    isa        => 'HashRef[My::W3C::SOAP::XSD::Document]',
    builder    => '_include',
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
        else {
            warn "Found import but no schemaLocation so no schema imported!\n\t" . $import->toString . "\n\t";
        }
    }

    return \@imports;
}

sub _imported {
    my ($self) = @_;
    my %import;
    for my $import (@{ $self->imports }) {
        $import{$import->name} = $import;
    }
    return \%import;
}

sub _includes {
    my ($self) = @_;
    my @includes;
    my @nodes = $self->xpc->findnodes('//xsd:include');

    for my $include (@nodes) {
        next if $include->getAttribute('namespace') && $include->getAttribute('namespace') eq 'http://www.w3.org/2001/XMLSchema';

        my $location = $include->getAttribute('schemaLocation');
        if ($location) {

            if ( $self->location && $self->location =~ m{^(?:https?|ftp)://} ) {
                $location = URI->new_abs($location, $self->location)->as_string;
            }

            push @includes, __PACKAGE__->new(
                location      => $location,
                ns_module_map => $self->ns_module_map,
                module_base   => $self->module_base,
            );
        }
        else {
            warn "Found include but no schemaLocation so no schema included!\n\t" . $include->toString . "\n\t";
        }
    }

    return \@includes;
}

sub _include {
    my ($self) = @_;
    my %include;
    for my $include (@{ $self->include }) {
        $include{$include->name} = $include;
    }
    return \%include;
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
            warn Dumper {
                document => $self,
                node     => $node,
            };
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
            warn Dumper {
                parent_node => $element->node->toString,
                document    => $self,
                node        => $node,
            };
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
    confess "No ns name\n".Dumper \%rev, $self->target_namespace if !$rev{$self->target_namespace};
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

    confess "Couldn't find the namespace '$ns_name' to map\nMap has:\n", Dumper $self->ns_map if !$self->ns_map->{$ns_name};

    return $self->ns_map->{$ns_name};
}

sub get_module_base {
    my ($self, $ns) = @_;

    confess "Trying to get module mappings when none specified!\n" if !$self->has_ns_module_map;
    confess "No mapping specified for the namespace $ns!\n"        if !$self->ns_module_map->{normalise_ns($ns)};

    return $self->ns_module_map->{normalise_ns($ns)};
}

1;
package My::W3C::SOAP::Parser;

# Created on: 2012-05-27 18:58:29
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use version;
our $VERSION     = version->new('0.02');

has document => (
    is       => 'rw',
    isa      => 'W3C::SOAP::Document',
);
has template => (
    is        => 'rw',
    isa       => 'Template',
    predicate => 'has_template',
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

package My::W3C::SOAP::XSD;

# Created on: 2012-05-26 23:50:44
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use warnings;
use version;
use Carp qw/carp croak cluck confess longmess/;
use Scalar::Util;
use List::Util;
#use List::MoreUtils;
use Data::Dumper qw/Dumper/;
use English qw/ -no_match_vars /;
use Moose::Util::TypeConstraints;
use MooseX::Types::XMLSchema;
use W3C::SOAP::XSD::Types qw/:all/;
use W3C::SOAP::Utils qw/split_ns/;
use TryCatch;
use DateTime::Format::Strptime qw/strptime/;

our $VERSION     = version->new('0.02');

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
                    my ($node_ns, $node) = split_ns($child->nodeName);
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

sub _from_xml {
    my ($class, $type) = @_;
    my $xml = $_;
    confess "Unknown conversion " . ( (ref $xml) || $xml )
        if !$xml || !blessed $xml || !$xml->isa('XML::LibXML::Node');

    try {
        return $type->new($xml);
    }
    catch ($e) {
        $e =~ s/ at .*//ms;
        warn "$class Failed in building from $type\->new($xml) : $e\n",
            "Will use :\n\t'",
            $xml->toString,
            "'\n\tor\n\t'",
            $xml->textContent,"'\n",
            '*' x 222,
            "\n";
    }
    return $xml->textContent;
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

sub to_xml {
    my ($self, $xml) = @_;
    my $child;
    my $meta = $self->meta;
    my @attributes = $self->get_xml_nodes;

    my @nodes;
    $self->clear_xsd_ns_name;
    my $xsd_ns_name = $self->xsd_ns ? $self->xsd_ns_name : undef;

    for my $att (@attributes) {
        my $name = $att->name;

        # skip attributes that are not XSD attributes
        next if !$att->does('My::W3C::SOAP::XSD');
        my $has = "has_$name";

        # skip sttributes that are not set
        next if !$self->$has;

        my $xml_name = $att->has_xs_name ? $att->xs_name : $name;

        my $value = ref $self->$name eq 'ARRAY' ? $self->$name : [$self->$name];

        for my $item (@$value) {
            my $tag = $xml->createElement($xsd_ns_name ? $xsd_ns_name . ':' . $xml_name : $xml_name);
            $tag->setAttribute("xmlns:$xsd_ns_name" => $self->xsd_ns) if $self->xsd_ns;

            if ( blessed($item) && $item->can('to_xml') ) {
                $item->xsd_ns_name( $xsd_ns_name ) if !$item->has_xsd_ns_name;
                my @children = $item->to_xml($xml);
                $tag->appendChild($_) for @children;
            }
            elsif ( ! defined $item && ! $att->has_xs_serialize ) {
                $tag->setAttribute('nil', 'true');
            }
            else {
                local $_ = $item;
                my $text
                    = $att->has_xs_serialize
                    ? $att->xs_serialize->($item)
                    : "$item";
                $tag->appendChild( $xml->createTextNode($text) );
            }

            push @nodes, $tag;
        }
    }

    return @nodes;
}

sub to_data {
    my ($self, %option) = @_;
    my $child;
    my $meta = $self->meta;
    my @attributes = $self->get_xml_nodes;

    my %nodes;

    for my $att (@attributes) {
        my $name = $att->name;

        # skip attributes that are not XSD attributes
        next if !$att->does('My::W3C::SOAP::XSD');
        my $has = "has_$name";

        # skip sttributes that are not set
        next if !$self->$has;

        my $key_name = $att->has_xs_name && $option{like_xml} ? $att->xs_name : $name;
        my $value = $self->$name;

        if ( ref $value eq 'ARRAY' ) {
            my @elements;
            for my $element (@$value) {
                if ( blessed($element) && $element->can('to_data') ) {
                    push @elements, $element->to_data(%option);
                }
            }
            $nodes{$key_name} = \@elements;
        }
        else {
            if ( blessed($value) && $value->can('to_data') ) {
                $value = $value->to_data(%option);
            }
            elsif ( ! defined $value && ! $att->has_xs_serialize ) {
            }
            elsif ($option{stringify}) {
                local $_ = $value;
                my $text
                    = $att->has_xs_serialize
                    ? $att->xs_serialize->($value)
                    : "$value";
                $value = defined $value ? $text : $value;
            }

            $nodes{$key_name} = $value;
        }
    }

    return \%nodes;
}

sub get_xml_nodes {
    my ($self) = @_;
    my $meta = $self->meta;

    my @parent_nodes;
    my @supers = $meta->superclasses;
    for my $super (@supers) {
        push @parent_nodes, $super->get_xml_nodes if $super ne __PACKAGE__ && UNIVERSAL::can($super, 'get_xml_nodes');
    }

    return @parent_nodes, map {
            $meta->get_attribute($_)
        }
        sort {
            $meta->get_attribute($a)->insertion_order <=> $meta->get_attribute($b)->insertion_order
        }
        grep {
            $meta->get_attribute($_)->does('W3C::SOAP::XSD::Traits')
        }
        $meta->get_attribute_list;
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
use warnings;
use version;
use List::MoreUtils qw/all/;
use Path::Class;
use File::ShareDir qw/dist_dir/;
use Moose::Util::TypeConstraints;

Moose::Exporter->setup_import_methods(
    as_is => ['load_xsd'],
);

extends 'W3C::SOAP::Parser';

our $VERSION     = version->new('0.02');

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

sub write_modules {
    my ($self) = @_;
    confess "No lib directory setup" if !$self->has_lib;
    confess "No template object set" if !$self->has_template;

    my @xsds     = $self->get_schemas;
    my $template = $self->template;
    my @schemas;
    my $self_module;
    my @parents;
    my @xsd_modules;

    # process the schemas
    for my $xsd (@xsds) {
        my $module = $xsd->module;
        push @xsd_modules, $module;
        $self_module ||= $module;
        my $file   = $self->lib . '/' . $module;
        $file =~ s{::}{/}g;
        $file = file $file;
        my $parent = $file->parent;
        my @missing;
        while ( !-d $parent ) {
            push @missing, $parent;
            $parent = $parent->parent;
        }
        mkdir $_ for reverse @missing;

        for my $type ( @{ $xsd->complex_types } ) {
            my $type_name = $type->name || $type->parent_node->name;
            warn  "me          = ".(ref $type).
                "\nnode        = ".($type->node->nodeName).
                "\nparent      = ".(ref $type->parent_node).
                "\nparent node = ".($type->node->parentNode->nodeName).
                "\ndocument    = ".(ref $type->document)."\n"
                if !$type_name;
            confess "No name found for ",
                $type->node->toString,
                "\nin :\n",
                $type->document->string,"\n"
                if !$type_name;
            my $type_module = $module . '::' . $type_name;
            push @parents, $type_module;
            my $type_file = $self->lib . '/' . $type_module;
            $type_file =~ s{::}{/}g;
            $type_file = file $type_file;
            mkdir $type_file->parent if !-d $type_file->parent;

            my %modules;
            for my $el (@{ $type->sequence }) {
                $modules{ $el->type_module }++
                    if ! $el->simple_type && $el->module ne $module
            }
            for my $element (@{ $type->sequence }) {
                next if $element->simple_type;
                my ($ns) = My::W3C::SOAP::Utils::split_ns($element->type);
                $ns ||= $element->document->target_namespace;
                my $ns_uri = $element->document->get_ns_uri($ns, $element->node);
                $modules{ $type->document->get_module_base($ns_uri) }++
                    if $ns_uri && $ns_uri ne $type->document->target_namespace;
            }

            # write the complex type module
            $self->write_module(
                'xsd/complex_type.pm.tt',
                {
                    xsd     => $xsd,
                    module  => $type_module,
                    modules => [ keys %modules ],
                    node    => $type
                },
                "$type_file.pm"
            );
        }

        # write the simple types library
        $self->write_module(
            'xsd/base.pm.tt',
            {
                xsd => $xsd,
            },
            "$file/Base.pm"
        );

        # write the "XSD" elements module
        $self->write_module(
            'xsd/pm.tt',
            {
                xsd => $xsd,
                parents => \@parents,
            },
            "$file.pm"
        );

    }

    return $self_module;
}

my %written;
sub write_module {
    my ($self, $tt, $data, $file) = @_;
    my $template = $self->template;

     if ($written{$file}++) {
        warn "Already written $file!\n";
        return;
    }

    $template->process($tt, $data, "$file");
    confess "Error in creating $file (via $tt): ". $template->error."\n"
        if $template->error;
}

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
        for my $include ( @{ $xsd->includes } ) {
            push @xsds, $include;
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

sub load_xsd {
    my ($location) = @_;
    my $parser = __PACKAGE__->new(
        location      => $location,
        ns_module_map => {},
    );

    return $parser->dynamic_classes;
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

        # Create simple types
        $self->simple_type_package($xsd);

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
            $self->complex_type_package($xsd, $type, $type_module, [ keys %modules ]);
        }

        # elements package
        $self->elements_package($xsd, $module);

        push @packages, $module;
    }

    return @packages;
}

sub simple_type_package {
    my ($self, $xsd) = @_;

    for my $subtype (@{ $xsd->simple_types }) {
        next if !$subtype->name;

        # Setup base simple types
        if ( @{ $subtype->enumeration } ) {
            enum(
                $subtype->moose_type
                => $subtype->enumeration
            );
        }
        else {
            subtype $subtype->moose_type =>
                as $subtype->moose_base_type;
        }

        # Add coercion from XML::LibXML nodes
        coerce $subtype->moose_type =>
            from 'XML::LibXML::Node' =>
            via { $_->textContent };
    }

    return;
}

sub complex_type_package {
    my ($self, $xsd, $type, $class_name, $super) = @_;

    my $class = Moose::Meta::Class->create(
        $class_name,
        superclasses => $super,
    );

    $class->add_attribute(
        '+xsd_ns',
        default  => $xsd->target_namespace,
        required => 1,
    );

    for my $node (@{ $type->sequence }) {
        $self->element_attributes($class, $class_name, $node);
    }

    return $class;
}

sub elements_package {
    my ($self, $xsd, $class_name) = @_;

    my $class = Moose::Meta::Class->create(
        $class_name,
        superclasses => [ 'My::W3C::SOAP::XSD' ],
    );

    $class->add_attribute(
        '+xsd_ns',
        default  => $xsd->target_namespace,
        required => 1,
    );

    for my $node (@{ $xsd->elements }) {
        $self->element_attributes($class, $class_name, $node);
    }

    return $class;
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

    if ( $ENV{W3C_SOAP_NAME_STYLE} eq 'both' && $element->name ne $element->perl_name ) {
        my $name = $element->perl_name;
        $class->add_method(
            $element->name => sub { shift->$name(@_) }
        );
    }

    return;
}

package My::W3C::SOAP::WSDL::Document::InOutPuts;

# Created on: 2012-05-28 07:30:02
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use warnings;
use version;
use Carp;
use Scalar::Util;
use List::Util;
#use List::MoreUtils;
use Data::Dumper qw/Dumper/;
use English qw/ -no_match_vars /;
use W3C::SOAP::Utils qw/split_ns/;
extends 'My::W3C::SOAP::Document::Node';

our $VERSION     = version->new('0.02');

has message => (
    is         => 'rw',
    isa        => 'Maybe[W3C::SOAP::WSDL::Document::Message]',
    builder    => '_message',
    lazy_build => 1,
);
has policy => (
    is         => 'rw',
    isa        => 'Maybe[Str]',
    builder    => '_policy',
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
    my ($ns, $message) = split_ns($self->node->getAttribute('message'));

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
use warnings;
use version;
use Carp;
use Scalar::Util;
use List::Util;
#use List::MoreUtils;
use Data::Dumper qw/Dumper/;
use English qw/ -no_match_vars /;

extends 'My::W3C::SOAP::Document::Node';

our $VERSION     = version->new('0.02');

has style => (
    is         => 'rw',
    isa        => 'Str',
    builder    => '_style',
    lazy_build => 1,
);
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
has faults => (
    is         => 'rw',
    isa        => 'ArrayRef[My::W3C::SOAP::WSDL::Document::InOutPuts]',
    builder    => '_faults',
    lazy_build => 1,
);
has port_type => (
    is         => 'rw',
    isa        => 'My::W3C::SOAP::WSDL::Document::Operation',
    builder    => '_port_type',
    lazy_build => 1,
);

sub _style {
    my ($self) = @_;
    my $style = $self->node->getAttribute('style');
    return $style if $style;
    my ($child) = $self->document->xpc->findnode('soap:binding'. $self->node);
    return $child->getAttribute('style');
}

sub _action {
    my ($self) = @_;
    my $action = $self->node->getAttribute('soapAction');
    return $action if $action;
    my ($child) = $self->document->xpc->findnode('soap:binding'. $self->node);
    return $child->getAttribute('soapAction');
}

sub _inputs  { return $_[0]->_in_out_puts('input');  }
sub _outputs { return $_[0]->_in_out_puts('output'); }
sub _faults  { return $_[0]->_in_out_puts('fault');  }
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

sub _port_type {
    my ($self) = @_;
    for my $port_type (@{ $self->document->port_types }) {
        for my $operation (@{ $port_type->operations }) {
            return $operation if $operation->name eq $self->name;
        }
    }
}

package My::W3C::SOAP::WSDL::Document::Binding;

# Created on: 2012-05-27 19:25:33
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use warnings;
use version;
use Carp;
use Scalar::Util;
use List::Util;
#use List::MoreUtils;
use Data::Dumper qw/Dumper/;
use English qw/ -no_match_vars /;

extends 'My::W3C::SOAP::Document::Node';

our $VERSION     = version->new('0.02');

has style => (
    is         => 'rw',
    isa        => 'Str',
    builder    => '_style',
    lazy_build => 1,
);
has transport => (
    is         => 'rw',
    isa        => 'Str',
    builder    => '_transport',
    lazy_build => 1,
);
has operations => (
    is         => 'rw',
    isa        => 'ArrayRef[My::W3C::SOAP::WSDL::Document::Operation]',
    builder    => '_operations',
    lazy_build => 1,
);

sub _style {
    my ($self) = @_;
    my $style = $self->node->getAttribute('style');
    return $style if $style;
    my ($child) = $self->document->xpc->findnode('soap:binding'. $self->node);
    return $child->getAttribute('style');
}

sub _transport {
    my ($self) = @_;
    my $transport = $self->node->getAttribute('transport');
    return $transport if $transport;
    my ($child) = $self->document->xpc->findnode('soap:binding'. $self->node);
    return $child->getAttribute('transport');
}

sub _operations {
    my ($self) = @_;
    my @operations;
    my @nodes = $self->document->xpc->findnodes('wsdl:operation', $self->node);

    for my $node (@nodes) {
        push @operations, My::W3C::SOAP::WSDL::Document::Operation->new(
            parent_node   => $self,
            node     => $node,
        );
    }

    return \@operations;
}

package My::W3C::SOAP::WSDL::Document::Message;

# Created on: 2012-05-27 19:25:15
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use warnings;
use version;
use Carp;
use Scalar::Util;
use List::Util;
#use List::MoreUtils;
use Data::Dumper qw/Dumper/;
use English qw/ -no_match_vars /;
use W3C::SOAP::Utils qw/split_ns xml_error cmp_ns/;

extends 'My::W3C::SOAP::Document::Node';

our $VERSION     = version->new('0.02');

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

    my ($ns, $el_name) = split_ns($element);
    my $nsuri = $self->document->get_nsuri($ns);
    my @schemas = @{ $self->document->schemas };

    for my $schema (@schemas) {
        push @schemas, @{ $schema->imports };
        push @schemas, @{ $schema->includes };

        if ( cmp_ns($schema->target_namespace, $nsuri) ) {
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

package My::W3C::SOAP::WSDL::Document::PortType;

# Created on: 2012-05-27 19:25:22
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use warnings;
use version;
use Carp;
use Scalar::Util;
use List::Util;
#use List::MoreUtils;
use Data::Dumper qw/Dumper/;
use English qw/ -no_match_vars /;

extends 'My::W3C::SOAP::Document::Node';

our $VERSION     = version->new('0.02');

has operations => (
    is         => 'rw',
    isa        => 'ArrayRef[My::W3C::SOAP::WSDL::Document::Operation]',
    builder    => '_operations',
    lazy_build => 1,
);

sub _operations {
    my ($self) = @_;
    my @operations;
    my @nodes = $self->document->xpc->findnodes('wsdl:operation', $self->node);

    for my $node (@nodes) {
        push @operations, My::W3C::SOAP::WSDL::Document::Operation->new(
            parent_node   => $self,
            node     => $node,
        );
    }

    return \@operations;
}

package My::W3C::SOAP::WSDL::Document::Service;

# Created on: 2012-05-27 19:25:41
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use warnings;
use version;
use Carp;
use Scalar::Util;
use List::Util;
#use List::MoreUtils;
use Data::Dumper qw/Dumper/;
use English qw/ -no_match_vars /;
use W3C::SOAP::WSDL::Document::Port;

extends 'My::W3C::SOAP::Document::Node';

our $VERSION     = version->new('0.02');

has ports => (
    is         => 'rw',
    isa        => 'ArrayRef[W3C::SOAP::WSDL::Document::Port]',
    builder    => '_ports',
    lazy_build => 1,
);

sub _ports {
    my ($self) = @_;
    my @complex_types;
    my @nodes = $self->document->xpc->findnodes('wsdl:port', $self->node);

    for my $node (@nodes) {
        push @complex_types, W3C::SOAP::WSDL::Document::Port->new(
            parent_node   => $self,
            node     => $node,
        );
    }

    return \@complex_types;
}

1;

package My::W3C::SOAP::WSDL::Document;

# Created on: 2012-05-27 18:57:29
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use warnings;
use version;
use Carp;
use Scalar::Util;
use List::Util;
#use List::MoreUtils;
use Data::Dumper qw/Dumper/;
use English qw/ -no_match_vars /;
use Path::Class;
use XML::LibXML;

extends 'W3C::SOAP::Document';

our $VERSION     = version->new('0.02');

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
has port_types => (
    is         => 'rw',
    isa        => 'ArrayRef[My::W3C::SOAP::WSDL::Document::PortType]',
    builder    => '_port_types',
    lazy_build => 1,
);
has port_type => (
    is         => 'rw',
    isa        => 'HashRef[My::W3C::SOAP::WSDL::Document::PortType]',
    builder    => '_port_type',
    lazy_build => 1,
    weak_ref   => 1,
);
has bindings => (
    is         => 'rw',
    isa        => 'ArrayRef[My::W3C::SOAP::WSDL::Document::Binding]',
    builder    => '_bindings',
    lazy_build => 1,
);
has binding => (
    is         => 'rw',
    isa        => 'HashRef[My::W3C::SOAP::WSDL::Document::Binding]',
    builder    => '_binding',
    lazy_build => 1,
    weak_ref   => 1,
);
has services => (
    is         => 'rw',
    isa        => 'ArrayRef[My::W3C::SOAP::WSDL::Document::Service]',
    builder    => '_services',
    lazy_build => 1,
);
has service => (
    is         => 'rw',
    isa        => 'HashRef[My::W3C::SOAP::WSDL::Document::Service]',
    builder    => '_service',
    lazy_build => 1,
    weak_ref   => 1,
);
has policies => (
    is         => 'rw',
    isa        => 'ArrayRef[W3C::SOAP::WSDL::Document::Policy]',
    builder    => '_policies',
    lazy_build => 1,
    weak_ref   => 1,
);
has policy => (
    is         => 'rw',
    isa        => 'HashRef[W3C::SOAP::WSDL::Document::Policy]',
    builder    => '_policy',
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

sub _port_types {
    my ($self) = @_;
    my @port_types;
    my @nodes = $self->xpc->findnodes('//wsdl:portType');

    for my $node (@nodes) {
        push @port_types, My::W3C::SOAP::WSDL::Document::PortType->new(
            document => $self,
            node   => $node,
        );
    }

    return \@port_types;
}

sub _port_type {
    my ($self) = @_;
    my %port_type;
    for my $port_type ( @{ $self->port_type }) {
        $port_type{$port_type->name} = $port_type;
    }

    return \%port_type;
}

sub _bindings {
    my ($self) = @_;
    my @bindings;
    my @nodes = $self->xpc->findnodes('//wsdl:binding');

    for my $node (@nodes) {
        push @bindings, My::W3C::SOAP::WSDL::Document::Binding->new(
            document => $self,
            node   => $node,
        );
    }

    return \@bindings;
}

sub _binding {
    my ($self) = @_;
    my %binding;
    for my $binding ( @{ $self->binding }) {
        $binding{$binding->name} = $binding;
    }

    return \%binding;
}

sub _services {
    my ($self) = @_;
    my @services;
    my @nodes = $self->xpc->findnodes('//wsdl:service');

    for my $node (@nodes) {
        push @services, My::W3C::SOAP::WSDL::Document::Service->new(
            document => $self,
            node   => $node,
        );
    }

    return \@services;
}

sub _service {
    my ($self) = @_;
    my %service;
    for my $service ( @{ $self->service }) {
        $service{$service->name} = $service;
    }

    return \%service;
}

sub _policies {
    my ($self) = @_;
    my @policies;
    my @nodes = $self->xpc->findnodes('/*/wsp:Policy');

    for my $node (@nodes) {
        push @policies, W3C::SOAP::WSDL::Document::Policy->new(
            document => $self,
            node     => $node,
        );
    }

    return \@policies;
}

sub _policy {
    my ($self) = @_;
    my %service;
    for my $service ( @{ $self->service }) {
        $service{$service->sec_id} = $service;
    }

    return \%service;
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

sub xsd_modules {
    my ($self) = @_;
    my %modules;

    for my $service (@{ $self->services }) {
        for my $port (@{ $service->ports }) {
            for my $operation (@{ $port->binding->operations }) {
                if ( $operation->port_type->outputs->[0] && $operation->port_type->outputs->[0]->message->element ) {
                    $modules{$operation->port_type->outputs->[0]->message->element->module}++;
                }
            }
        }
    }

    return ( sort keys %modules );
}

package My::W3C::SOAP::WSDL::Parser;

# Created on: 2012-05-27 18:58:29
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use warnings;
use version;
#use List::MoreUtils;
use Path::Class;
use W3C::SOAP::WSDL::Meta::Method;
use File::ShareDir qw/dist_dir/;

extends 'My::W3C::SOAP::Parser';

our $VERSION     = version->new('0.02');

has '+document' => (
    isa      => 'My::W3C::SOAP::WSDL::Document',
    required => 1,
    handles  => {
        module          => 'module',
        has_module      => 'has_module',
        ns_module_map   => 'ns_module_map',
        module_base     => 'module_base',
        has_module_base => 'has_module_base',
    },
);
has location => (
    is  => 'rw',
    isa => 'Str',
);

sub write_modules {
    my ($self) = @_;
    confess "No lib directory setup" if !$self->has_lib;
    confess "No module name setup"   if !$self->has_module;
    confess "No template object set" if !$self->has_template;

    my $wsdl = $self->document;
    my $template = $self->template;
    my $file     = $self->lib . '/' . $self->module . '.pm';
    $file =~ s{::}{/}g;
    $file = file $file;
    my $parent = $file->parent;
    my @missing;
    while ( !-d $parent ) {
        push @missing, $parent;
        $parent = $parent->parent;
    }
    mkdir $_ for reverse @missing;
    my @modules = $self->get_xsd->write_modules;

    confess "No XSD modules found!\n" unless @modules;

    my $data = {
        wsdl     => $wsdl,
        module   => $self->module,
        xsd      => shift @modules,
        modules  => \@modules,
        location => $self->location,
    };
    $template->process('wsdl/pm.tt', $data, "$file");
    confess "Error in creating $file (xsd.pm): ". $template->error."\n"
        if $template->error;

}

sub get_xsd {
    my ($self) = @_;

    my @args;
    push @args, ( template      => $self->template ) if $self->has_template;
    push @args, ( lib           => $self->lib      ) if $self->has_lib     ;
    if ( $self->has_module_base ) {
        my $base = $self->module_base;
        $base =~ s/WSDL/XSD/;
        $base .= '::XSD' if ! $base =~ /XSD/;
        push @args, ( module_base => $base );
    }

    my $parse = My::W3C::SOAP::XSD::Parser->new(
        document      => [],
        ns_module_map => $self->ns_module_map,
        @args,
    );

    for my $xsd (@{ $self->document->schemas }) {
        $xsd->ns_module_map($self->ns_module_map);
        $xsd->clear_xpc;

        push @{ $parse->document }, $xsd;

        $parse->document->[-1]->target_namespace($self->document->target_namespace)
            if !$parse->document->[-1]->has_target_namespace;
    }

    return $parse;
}

my %cache;
sub load_wsdl {
    my ($location) = @_;

    return $cache{$location} if $cache{$location};

    my $parser = __PACKAGE__->new(
        location => $location,
        ns_module_map => {},
    );

    my $class = $parser->dynamic_classes;

    return $cache{$location} = $class->new;
}

sub dynamic_classes {
    my ($self) = @_;
    my @classes = $self->get_xsd->dynamic_classes;

    my $class_name = "Dynamic::WSDL::" . My::W3C::SOAP::Utils::ns2module($self->document->target_namespace);

    my $wsdl = $self->document;
    my %method;
    for my $service (@{ $wsdl->services }) {
        for my $port (@{ $service->ports }) {
            for my $operation (@{ $port->binding->operations }) {
                my $in_element  = eval { $operation->port_type->inputs->[0]->message->element };
                my $out_element = eval { $operation->port_type->outputs->[0]->message->element };
                my @faults = eval {
                    map {{
                        class => $_->message->element->module,
                        name  => $_->message->element->perl_name,
                    }}
                    @{ $operation->port_type->faults }
                };

                $method{ $operation->perl_name } = W3C::SOAP::WSDL::Meta::Method->wrap(
                    body           => sub { shift->_request($operation->perl_name => @_) },
                    package_name   => $class_name,
                    name           => $operation->perl_name,
                    wsdl_operation => $operation->name,
                    $in_element  ? ( in_class      => $in_element->module     ) : (),
                    $in_element  ? ( in_attribute  => $in_element->perl_name  ) : (),
                    $out_element ? ( out_class     => $out_element->module    ) : (),
                    $out_element ? ( out_attribute => $out_element->perl_name ) : (),
                    @faults ? ( faults => \@faults ) : (),
                );

                if ( $ENV{W3C_SOAP_NAME_STYLE} eq 'both' && $operation->name ne $operation->perl_name ) {
                    my $name = $operation->perl_name;
                    $method{ $operation->name } = Moose::Meta::Method->wrap(
                        body         => sub { shift->$name(@_) },
                        package_name => $class_name,
                        name         => $operation->name,
                    );
                }
            }
        }
    }

    my $class = Moose::Meta::Class->create(
        $class_name,
        superclasses => [ 'W3C::SOAP::WSDL' ],
        methods      => \%method,
    );

    $class->add_attribute(
        '+location',
        default  => $wsdl->services->[0]->ports->[0]->address,
        required => 1,
    );

    return $class_name;
}

1;

package main;

# set up templates
my $template = Template->new(
    INCLUDE_PATH => dist_dir('W3C-SOAP').':./templates',
    INTERPOLATE  => 0,
    EVAL_PERL    => 1,
);
# create the parser object
my $parser = My::W3C::SOAP::WSDL::Parser->new(
    location      => 't/eg.wsdl',
    module        => 'MyApp::WsdlEg',
    template      => $template,
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
ok scalar( @{ $parser->document->port_types } ), "Got some port types";

done_testing();
exit;

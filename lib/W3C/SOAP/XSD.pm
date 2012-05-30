package W3C::SOAP::XSD;

# Created on: 2012-05-26 23:50:44
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
use Moose::Util::TypeConstraints;
use MooseX::Types::XMLSchema;
use W3C::SOAP::XSD::Traits;

our $VERSION     = version->new('0.0.1');
our @EXPORT_OK   = qw//;
our %EXPORT_TAGS = ();
#our @EXPORT      = qw//;

has xsd_ns => (
    is  => 'rw',
    isa => 'Str',
);
has xsd_ns_name => (
    is         => 'rw',
    isa        => 'Str',
    predicate  => 'has_xsd_ns_name',
    builder    => '_xsd_ns_name',
    lazy_build => 1,
);

my %ns_map;
my $count = 0;
sub _xsd_ns_name {
    my ($self) = @_;
    my $ns = $self->xsd_ns;

    return $ns_map{$ns} if $ns_map{$ns};

    return $ns_map{$ns} = 'WSX' . $count++;
}

sub to_xml {
    my ($self, $xml) = @_;
    my $child;
    my $meta = $self->meta;

    warn ref $self, " converting to XML\n";
    my @attributes = sort {
            $meta->get_attribute($a)->insertion_order <=> $meta->get_attribute($b)->insertion_order
        }
        grep {
            $meta->get_attribute($_)->does('W3C::SOAP::XSD::Traits')
        }
        $meta->get_attribute_list;

    warn 'XSD object: ', Dumper \@attributes;

    my @nodes;

    for my $name (@attributes) {
        #warn $name, "\n";
        my $att = $meta->get_attribute($name);

        # skip attributes that are not XSD attributes
        next if !$att->does('W3C::SOAP::XSD');
        my $has = "has_$name";

        # skip sttributes that are not set
        next if !$self->$has;
        #warn "have $name\n";

        my $xml_name = $att->does('NScreens::SDPx::XSD::Traits') && $att->has_xml_name ? $att->xml_name : $name;
        #warn $att->type_constraint;
        my $xsd_ns_name = $self->xsd_ns_name;
        my $tag = $xml->createElement($xsd_ns_name . ':' . $xml_name);
        $tag->setAttribute("xmlns:$xsd_ns_name" => $self->xsd_ns) if $self->xsd_ns;

        my $value = $self->$name;

        if ( blessed($value) && $value->can('to_xml') ) {
            $value->xsd_ns_name( $xsd_ns_name ) if !$value->has_xsd_ns_name;
            $tag->appendChild($value->to_xml($xml));
        }
        else {
            $tag->appendChild( $xml->createTextNode("$value") );
        }

        #warn $tag->toString, "\n";
        push @nodes, $tag;
    }

    return @nodes;
}

sub to_data {
}

sub xsd_subtype {
    my ($self, $type, %args) = @_;

    my $subtype = subtype as $args{list} ? "ArrayRef[$type]" : $type;
    coerce $subtype =>
        from 'xml_node' =>
        via { eval { $type->new($_) } || $_->toString };

    if ( $args{list} ) {
        coerce $subtype =>
            from 'MyApp::XSD::TpsCommonTypes::ProductFamilyType',
            via {[$_]};
        coerce $subtype =>
            from 'ArrayRef[xml_node]' =>
            via { [ map { eval { $type->new($_) } || $_->toString } @$_ ] };
    }

    return $subtype;
}

1;

__END__

=head1 NAME

W3C::SOAP::XSD - <One-line description of module's purpose>

=head1 VERSION

This documentation refers to W3C::SOAP::XSD version 0.1.


=head1 SYNOPSIS

   use W3C::SOAP::XSD;

   # Brief but working code example(s) here showing the most common usage(s)
   # This section will be as far as many users bother reading, so make it as
   # educational and exemplary as possible.


=head1 DESCRIPTION

A full description of the module and its features.

May include numerous subsections (i.e., =head2, =head3, etc.).


=head1 SUBROUTINES/METHODS

A separate section listing the public components of the module's interface.

These normally consist of either subroutines that may be exported, or methods
that may be called on objects belonging to the classes that the module
provides.

Name the section accordingly.

In an object-oriented module, this section should begin with a sentence (of the
form "An object of this class represents ...") to give the reader a high-level
context to help them understand the methods that are subsequently described.




=head1 DIAGNOSTICS

A list of every error and warning message that the module can generate (even
the ones that will "never happen"), with a full explanation of each problem,
one or more likely causes, and any suggested remedies.

=head1 CONFIGURATION AND ENVIRONMENT

A full explanation of any configuration system(s) used by the module, including
the names and locations of any configuration files, and the meaning of any
environment variables or properties that can be set. These descriptions must
also include details of any configuration language used.

=head1 DEPENDENCIES

A list of all of the other modules that this module relies upon, including any
restrictions on versions, and an indication of whether these required modules
are part of the standard Perl distribution, part of the module's distribution,
or must be installed separately.

=head1 INCOMPATIBILITIES

A list of any modules that this module cannot be used in conjunction with.
This may be due to name conflicts in the interface, or competition for system
or program resources, or due to internal limitations of Perl (for example, many
modules that use source code filters are mutually incompatible).

=head1 BUGS AND LIMITATIONS

A list of known problems with the module, together with some indication of
whether they are likely to be fixed in an upcoming release.

Also, a list of restrictions on the features the module does provide: data types
that cannot be handled, performance issues and the circumstances in which they
may arise, practical limitations on the size of data sets, special cases that
are not (yet) handled, etc.

The initial template usually just has:

There are no known bugs in this module.

Please report problems to Ivan Wills (ivan.wills@gmail.com).

Patches are welcome.

=head1 AUTHOR

Ivan Wills - (ivan.wills@gmail.com)
<Author name(s)>  (<contact address>)

=head1 LICENSE AND COPYRIGHT

Copyright (c) 2012 Ivan Wills (14 Mullion Close, Hornsby Heights, NSW Australia 2077).
All rights reserved.

This module is free software; you can redistribute it and/or modify it under
the same terms as Perl itself. See L<perlartistic>.  This program is
distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.

=cut

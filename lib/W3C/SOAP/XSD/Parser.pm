package W3C::SOAP::XSD::Parser;

# Created on: 2012-05-28 08:11:37
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use version;
use Carp;
use Scalar::Util;
use List::Util;
#use List::MoreUtils;
use Data::Dumper qw/Dumper/;
use English qw/ -no_match_vars /;
use Path::Class;
use W3C::SOAP::XSD::Document;
use File::ShareDir qw/dist_dir/;
use Moose::Util::TypeConstraints;

our $VERSION     = version->new('0.0.1');
our @EXPORT_OK   = qw//;
our %EXPORT_TAGS = ();
#our @EXPORT      = qw//;

subtype xsd_documents =>
    as 'ArrayRef[W3C::SOAP::XSD::Document]';
coerce xsd_documents =>
    from 'W3C::SOAP::XSD::Document',
    via {[$_]};
has documents => (
    is       => 'rw',
    isa      => 'xsd_documents',
    coerce   => 1,
);
has template => (
    is       => 'rw',
    isa      => 'Template',
    required => 1,
);
has ns_module_map => (
    is       => 'rw',
    isa      => 'HashRef[Str]',
    required => 1,
);
has lib => (
    is       => 'rw',
    isa      => 'Str',
    required => 1,
);

around BUILDARGS => sub {
    my ($orig, $class, @args) = @_;
    my $args
        = !@args     ? {}
        : @args == 1 ? $args[0]
        :              {@args};

    for my $arg ( keys %$args ) {
        if ( $arg eq 'location' || $arg eq 'string' ) {
            $args->{documents} = W3C::SOAP::XSD::Document->new($args);
        }
    }

    return $class->$orig($args);
};

sub write_modules {
    my ($self) = @_;
    my @xsds     = @{ $self->documents };
    my $template = $self->template;
    my @schemas;
    my $self_module;
    my @parents;
    my @xsd_modules;
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

    # process the schemas
    for my $xsd (@xsds) {
        my $module = $xsd->get_module_base($xsd->target_namespace);
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

            $template->process('xsd_complex_type.pm.tt', {xsd => $xsd, module => $type_module, node => $type}, "$type_file.pm");
            die "Error in creating $type_file (xsd_complex_type.pm.tt): ". $template->error."\n"
                if $template->error;
        }

        $template->process('xsd_base.pm.tt', {xsd => $xsd}, "$file/Base.pm");
        die "Error in creating $file/Base.pm (xsd_base.pm): ". $template->error."\n"
            if $template->error;

        $template->process('xsd.pm.tt', {xsd => $xsd, parents => \@parents}, "$file.pm");
        die "Error in creating $file (xsd.pm): ". $template->error."\n"
            if $template->error;

    }

    #warn Dumper \@xsd_modules, $self_module;
    return $self_module;
}

my %written;
sub write_module {
    my ($self, $tt, $data, $file) = @_;
    my $template = $self->template;
    confess "Already written $file!\n" if $written{$file}++;

    $template->process($tt, $data, "$file");
    die "Error in creating $file (via $tt): ". $template->error."\n"
        if $template->error;
}

1;

__END__

=head1 NAME

W3C::SOAP::XSD::Parser - <One-line description of module's purpose>

=head1 VERSION

This documentation refers to W3C::SOAP::XSD::Parser version 0.1.

=head1 SYNOPSIS

   use W3C::SOAP::XSD::Parser;

   # Brief but working code example(s) here showing the most common usage(s)
   # This section will be as far as many users bother reading, so make it as
   # educational and exemplary as possible.


=head1 DESCRIPTION

=head1 SUBROUTINES/METHODS

=over 4

=item C<write_modules ()>

Uses the supplied documents to write out perl modules to disk that represent
the XSDs in the documents.

=item C<write_module ($tt, $data, $file)>

Write the template to disk

=back

=head1 DIAGNOSTICS

=head1 CONFIGURATION AND ENVIRONMENT

=head1 DEPENDENCIES

=head1 INCOMPATIBILITIES

=head1 BUGS AND LIMITATIONS

There are no known bugs in this module.

Please report problems to Ivan Wills (ivan.wills@gmail.com).

Patches are welcome.

=head1 AUTHOR

Ivan Wills - (ivan.wills@gmail.com)

=head1 LICENSE AND COPYRIGHT

Copyright (c) 2012 Ivan Wills (14 Mullion Close, Hornsby Heights, NSW Australia 2077).
All rights reserved.

This module is free software; you can redistribute it and/or modify it under
the same terms as Perl itself. See L<perlartistic>.  This program is
distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.

=cut

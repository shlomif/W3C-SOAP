#!/usr/bin/perl

use strict;
use warnings;

use Test::More;
use File::ShareDir qw/dist_dir/;
use Template;

package MyW3C::Utils;

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

package MyW3C::Parser;

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

1;

package MyW3C::XSD::Parser;

# Created on: 2012-05-28 08:11:37
# Create by:  Ivan Wills
# $Id$
# $Revision$, $HeadURL$, $Date$
# $Revision$, $Source$, $Date$

use Moose;
use warnings;
use version;
use List::MoreUtils qw/all/;
use Data::Dumper qw/Dumper/;
use Path::Class;
use W3C::SOAP::XSD::Document;
use File::ShareDir qw/dist_dir/;
use Moose::Util::TypeConstraints;
use W3C::SOAP::XSD;

Moose::Exporter->setup_import_methods(
    as_is => ['load_xsd'],
);

extends 'W3C::SOAP::Parser';

our $VERSION     = version->new('0.02');

subtype xsd_documents =>
    as 'ArrayRef[W3C::SOAP::XSD::Document]';
coerce xsd_documents =>
    from 'W3C::SOAP::XSD::Document',
    via {[$_]};
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
                my ($ns) = MyW3C::Utils::split_ns($element->type);
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

    #warn Dumper \@xsd_modules, $self_module;
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

    my %complex_seen = ( 'W3C::SOAP::XSD' => 1 );
    for my $xsd (@ordered_xsds) {
        my $module = $xsd->module;

        # Create simple types
        $self->simple_type_package($xsd);

        # Complex types
        my @complex_types = @{ $xsd->complex_types };
        while ( my $type = shift @complex_types ) {
            my $type_name = $type->name || $type->parent_node->name;
            my $type_module = $module . '::' . $type_name;

            my %modules = ( 'W3C::SOAP::XSD' => 1 );
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
        superclasses => [ 'W3C::SOAP::XSD' ],
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
        traits        => [qw{ W3C::SOAP::XSD }],
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
1;
package W3C::SOAP::WSDL::Parser;

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
use W3C::SOAP::WSDL::Document;
use W3C::SOAP::WSDL::Meta::Method;
use File::ShareDir qw/dist_dir/;

extends 'MyW3C::Parser';

our $VERSION     = version->new('0.02');

has '+document' => (
    isa      => 'W3C::SOAP::WSDL::Document',
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

    my $parse = MyW3C::XSD::Parser->new(
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

    my $class_name = "Dynamic::WSDL::" . MyW3C::Utils::ns2module($self->document->target_namespace);

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
my $parser = W3C::SOAP::WSDL::Parser->new(
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

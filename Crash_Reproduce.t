#!/usr/bin/perl

use strict;
use warnings;
use Test::More;
use Path::Class;
use File::ShareDir qw/dist_dir/;
use Template;
use W3C::SOAP::WSDL::Parser;

my $dir = dir('./t');

plan( skip_all => 'Test can only be run if test directory is writable' ) if !-w $dir;

# set up templates
my $template = Template->new(
    INCLUDE_PATH => dist_dir('W3C-SOAP').':'.$dir->subdir('../templates'),
    INTERPOLATE  => 0,
    EVAL_PERL    => 1,
);
# create the parser object
my $parser = W3C::SOAP::WSDL::Parser->new(
    location      => $dir->file('eg.wsdl').'',
    module        => 'MyApp::WsdlEg',
    template      => $template,
    lib           => $dir->subdir('lib').'',
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

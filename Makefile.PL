use ExtUtils::MakeMaker;
WriteMakefile(
    NAME         => 'Tree::Simple::Manager',
    VERSION_FROM => 'lib/Tree/Simple/Manager.pm', 
    PREREQ_PM    => {
        'Test::More'         => 0.47,
        'Test::Exception'    => 0.15,
        'Tree::Simple'       => 1.14,
        'Tree::Parser'       => 0.09,
        'Tree::Simple::View' => 0.11,
        'Class::Throwable'   => 0.10
    }
);

my $filename = 'input';

open(FH, '<', $filename) or die $!;

while(<FH>) {
    print $_;
}

close(FH);
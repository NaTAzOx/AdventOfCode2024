use strict;
use warnings;

my $filename = 'December10/input';

open(FH, '<', $filename) or die $!;
my @lines = <FH>;
close(FH);

chomp @lines;

my @grid = map { [split //, $_] } @lines;

my $rows = scalar @grid;
my $cols = scalar @{$grid[0]};
my $counter = 0;

my @directions = ([0, 1], [1, 0], [0, -1], [-1, 0]);

sub in_bounds {
    my ($x, $y) = @_;
    return $x >= 0 && $x < $cols && $y >= 0 && $y < $rows;
}

sub find_paths {
    my ($x, $y, $value, $visited) = @_;
    if ($value == 9) {
        $counter++;
        return;
    }
    $visited->{"$x,$y"} = 1;
    foreach my $dir (@directions) {
        my ($dx, $dy) = @$dir;
        my $nx = $x + $dx;
        my $ny = $y + $dy;
        if (in_bounds($nx, $ny) && !$visited->{"$nx,$ny"} && $grid[$ny][$nx] == $value + 1) {
            find_paths($nx, $ny, $value + 1, $visited);
        }
    }
    delete $visited->{"$x,$y"};
}

for (my $i = 0; $i < $rows; $i++) {
    for (my $j = 0; $j < $cols; $j++) {
        if ($grid[$i][$j] == 0) {
            my %visited;
            find_paths($j, $i, 0, \%visited);
        }
    }
}

print "Number of paths from 0 to 9: $counter\n";
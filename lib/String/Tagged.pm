#  You may distribute under the terms of either the GNU General Public License
#  or the Artistic License (the same terms as Perl itself)
#
#  (C) Paul Evans, 2008-2010 -- leonerd@leonerd.org.uk

package String::Tagged;

use strict;
use warnings;

use constant FLAG_ANCHOR_BEFORE => 0x01;
use constant FLAG_ANCHOR_AFTER  => 0x02;

our $VERSION = '0.06';

=head1 NAME

C<String::Tagged> - string buffers with value tags on extents

=head1 SYNOPSIS

 use String::Tagged;

 my $st = String::Tagged->new( "An important message" );

 $st->apply_tag( 3, 9, bold => 1 );

 $st->iter_substr_nooverlap(
    sub {
       my ( $substring, %tags ) = @_;

       print $tags{bold} ? "<b>$substring</b>"
                         : $substring;
    }
 );

=head1 DESCRIPTION

This module implements an object class, instances of which store a (mutable)
string buffer that supports tags. A tag is a name/value pair that applies to
some non-empty extent of the underlying string.

The types of tag names ought to be strings, or at least values that are
well-behaved as strings, as the names will often be used as the keys in hashes
or applied to the C<eq> operator.

The types of tag values are not restricted - any scalar will do. This could be
a simple integer or string, ARRAY or HASH reference, or even a CODE reference
containing an event handler of some kind.

Tags may be arbitrarily overlapped. Any given offset within the string has in
effect, a set of uniquely named tags. Tags of different names are independent.
For tags of the same name, only the the latest, shortest tag takes effect.

For example, consider a string with three tags represented here:

 Here is my string with tags
 [-------------------------]  foo => 1
         [-------]            foo => 2
      [---]                   bar => 3

Every character in this string has a tag named C<foo>. The value of this tag
is 2 for the words C<my> and C<string> and the space inbetween, and 1
elsewhere. Additionally, the words C<is> and C<my> and the space between them
also have the tag C<bar> with a value 3.

Since C<String::Tagged> does not understand the significance of the tag values
it therefore cannot detect if two neighbouring tags really contain the same
semantic idea. Consider the following string:

 A string with words
 [-------]            type => "message"
          [--------]  type => "message"

This string contains two tags. C<String::Tagged> will treat this as two
different tag values as far as C<iter_tags_nooverlap()> is concerned, even
though C<get_tag_at()> yields the same value for the C<type> tag at any
position in the string.

=head1 NAMING

I spent a lot of time considering the name for this module. It seems that a
number of people across a number of languages all created similar
functionallity, though named very differently. For the benefit of
keyword-based search tools and similar, here's a list of some other names this
sort of object might be known by:

=over 4

=item *

Extents

=item *

Overlays

=item *

Attribute or attributed strings

=item *

Markup

=item *

Out-of-band data

=back

=cut

=head1 CONSTRUCTOR

=cut

=head2 $st = String::Tagged->new( $str )

Returns a new instance of a C<String::Tagged> object. It will contain no tags.
If the optional C<$str> argument is supplied, the string buffer will be
initialised from this value.

If C<$str> is a C<String::Tagged> object then its tags will be copied too;
i.e. the constructor also works as a clone method.

=cut

sub new
{
   my $class = shift;
   my ( $str ) = @_;

   $str = "" unless defined $str;

   my $self = bless {
      str  => "$str",
      tags => [],
   }, $class;

   if( eval { $str->isa( __PACKAGE__ ) } ) {
      $str->iter_extents( sub {
         my ( $e, $tn, $tv ) = @_;
         $self->apply_tag(
            $e->anchor_before ? -1 : $e->start,
            $e->anchor_after  ? -1 : $e->length,
            $tn, $tv );
      } );
   }

   return $self;
}

sub _mkextent
{
   my $self = shift;
   my ( $start, $end, $flags ) = @_;

   $flags &= (FLAG_ANCHOR_BEFORE|FLAG_ANCHOR_AFTER);

   return bless [ $self, $start, $end, $flags ], 'String::Tagged::Extent';
}

=head1 METHODS

=cut

=head2 $str = $st->str

=head2 "$st"

Returns the plain string contained within the object.

This method is also called for stringification; so the C<String::Tagged>
object can be used in a plain string interpolation such as

 my $message = String::Tagged->new( "Hello world" );
 print "My message is $message\n";

=cut

use overload '""' => 'str';

sub str
{
   my $self = shift;
   return $self->{str};
}

=head2 $len = $st->length

=head2 $len = length( $st )

Returns the length of the plain string. Because stringification works on this
object class, the normal core C<length()> function works correctly on it.

=cut

sub length
{
   my $self = shift;
   return CORE::length $self->{str};
}

=head2 $str = $st->substr( $start, $len )

Returns a substring of the plain string contained within the object.

=cut

sub substr
{
   my $self = shift;
   my ( $start, $len ) = @_;

   return CORE::substr( $self->{str}, $start, $len );
}

sub _cmp_tags
{
   my ( $as, $ae ) = @$a;
   my ( $bs, $be ) = @$b;

   # Sort by start first; shortest first
   return $as <=> $bs ||
          $ae <=> $be;
}

sub _assert_sorted
{
   my $self = shift;

   my $tags = $self->{tags};
   # If fewer than 2 tags, must be sorted
   return if @$tags < 2;

   my $prev = $tags->[0];

   for( my $i = 1; $i < @$tags; $i++ ) {
      my $here = $tags->[$i];
      local ( $a, $b ) = ( $prev, $here );
      if( _cmp_tags() <= 0 ) {
         $prev = $here;
         next;
      }

      print STDERR "Tag order violation at i=$i\n";
      print STDERR "[@{[ $i - 1 ]}] = [ $tags->[$i-1]->[0], $tags->[$i-1]->[1] ]\n";
      print STDERR "[@{[ $i     ]}] = [ $tags->[$i]->[0], $tags->[$i]->[1] ]\n";
      die "Assert failure";
   }
}

sub _insert_tag
{
   my $self = shift;
   my ( $start, $end, $name, $value, $flags ) = @_;

   my $tags = $self->{tags};

   my $newtag = [ $start, $end, $name => $value, $flags ];

   # Specialcase - if there's no tags yet, just push it
   if( @$tags == 0 ) {
      push @$tags, $newtag;
      return;
   }

   local $a = $newtag;

   # Two more special cases - it's quite likely we're either inserting an
   # 'everywhere' tag, or appending one to the end. Check the endpoints first
   local $b;
   
   $b = $tags->[0];
   if( _cmp_tags() <= 0 ) {
      unshift @$tags, $newtag;
      return;
   }

   $b = $tags->[-1];
   if( _cmp_tags() >= 0 ) {
      push @$tags, $newtag;
      return;
   }

   my $range_start = 0;
   my $range_end = $#$tags;

   my $inspos;

   while( $range_end > $range_start ) {
      my $i = int( ( $range_start + $range_end ) / 2 );

      $b = $tags->[$i];
      my $cmp = _cmp_tags;

      if( $cmp > 0 ) {
         $range_start = $i + 1;
      }
      elsif( $cmp < 0 ) {
         $range_end = $i; # open interval
      }
      else {
         $inspos = $i;
         last;
      }

      if( $range_start == $range_end ) {
         $inspos = $range_start;
         last;
      }
   }

   $inspos = $range_end unless defined $inspos;

   $inspos = 0 if $inspos < 0;
   $inspos = @$tags if $inspos > @$tags;

   splice @$tags, $inspos, 0, $newtag;

   $self->_assert_sorted;
}

=head2 $st->apply_tag( $start, $len, $name, $value )

Apply the named tag value to the given extent. The tag will start on the
character at the C<$start> index, and continue for the next C<$len>
characters.

If C<$start> is given as -1, the tag will be considered to start "before" the
actual string. If C<$len> is given as -1, the tag will be considered to
end "after" end of the actual string. These special limits are used by
C<set_substr()> when deciding whether to move a tag boundary. The start of any
tag that starts "before" the string is never moved, even if more text is
inserted at the beginning. Similarly, a tag which ends "after" the end of the
string, will continue to the end even if more text is appended.

=cut

sub apply_tag
{
   my $self = shift;
   my ( $start, $len, $name, $value ) = @_;

   my $strlen = $self->length;

   my $flags = 0;

   if( $start < 0 ) {
      $start = 0;
      $flags |= FLAG_ANCHOR_BEFORE;
   }

   my $end;
   if( $len == -1 ) {
      $end = $strlen;
      $flags |= FLAG_ANCHOR_AFTER;
   }
   else {
      $end = $start + $len;
      $end = $strlen if $end > $strlen;
   }

   $self->_insert_tag( $start, $end, $name, $value, $flags );
}

sub _remove_tag
{
   my $self = shift;
   my ( $start, $len, $name, $keepends ) = @_;

   my $end = $start + $len;

   my $tags = $self->{tags};

   my $have_added = 0;

   # Can't foreach() because we modify $i
   for( my $i = 0; $i < @$tags; $i++ ) {
      my ( $ts, $te, $tn, $tv, $tf ) = @{ $tags->[$i] };

      next if $te <= $start;
      last if $ts >= $end;

      next if $tn ne $name;

      if( $keepends and $end < $te ) {
         $self->_insert_tag( $end, $te, $tn, $tv, $tf & ~FLAG_ANCHOR_BEFORE );
         $have_added = 1;
      }

      splice @$tags, $i, 1;

      if( $keepends and $ts < $start ) {
         $self->_insert_tag( $ts, $start, $tn, $tv, $tf & ~FLAG_ANCHOR_AFTER );
         $have_added = 1;
      }
      else {
         $i--;
      }
   }

   if( $have_added ) {
      $self->_assert_sorted;
   }
}

=head2 $st->unapply_tag( $start, $len, $name )

Unapply the named tag value from the given extent. If the tag extends beyond
this extent, then any partial fragment of the tag will be left in the string.

=cut

sub unapply_tag
{
   my $self = shift;
   my ( $start, $len, $name ) = @_;
   $self->_remove_tag( $start, $len, $name, 1 );
}

=head2 $st->delete_tag( $start, $len, $name )

Delete the named tag within the given extent. Entire tags are removed, even if
they extend beyond this extent.

=cut

sub delete_tag
{
   my $self = shift;
   my ( $start, $len, $name ) = @_;
   $self->_remove_tag( $start, $len, $name, 0 );
}

=head2 $st->merge_tags( $eqsub )

Merge neighbouring or overlapping tags of the same name and equal values.

For each pair of tags of the same name that apply on neighbouring or
overlapping extents, the C<$eqsub> callback is called, as

  $equal = $eqsub->( $name, $value_a, $value_b )

If this function returns true then the tags are merged.

The equallity test function is free to perform any comparison of the values
that may be relevant to the application; for example it may deeply compare
referred structures and check for equivalence in some application-defined
manner. In this case, the first tag of a pair is retained, the second is
deleted. This may be relevant if the tag value is a reference to some object.

=cut

sub merge_tags
{
   my $self = shift;
   my ( $eqsub ) = @_;

   my $tags = $self->{tags};

   # Can't foreach() because we modify @$tags
   OUTER: for( my $i = 0; $i < @$tags; $i++ ) {
      my ( $ts, $te, $tn, $tv, $tf ) = @{ $tags->[$i] };

      for( my $j = $i+1; $j < @$tags; $j++ ) {
         my ( $t2s, $t2e, $t2n, $t2v, $t2f ) = @{ $tags->[$j] };

         last if $t2s > $te;
         next unless $t2s <= $te;
         next unless $t2n eq $tn;

         last unless $eqsub->( $tn, $tv, $t2v );

         # Need to delete the tag at $j, extend the end of the tag at $i, and
         # possibly move $i later
         splice @$tags, $j, 1, ();
         $j--;

         $te = $tags->[$i][1] = $t2e;

         $tags->[$i][4] |= FLAG_ANCHOR_AFTER if $t2f & FLAG_ANCHOR_AFTER;

         local $a = $tags->[$i];

         if( local $b = $tags->[$i+1] and _cmp_tags() > 0 ) {
            my $newpos = $i+1;
            while( local $b = $tags->[$newpos ] and _cmp_tags() <= 0 ) {
               $newpos++;
            }

            splice @$tags, $newpos, 0, splice @$tags, $i, 1, ();

            redo OUTER;
         }
      }
   }
}

=head2 $st->iter_extents( $callback, %opts )

Iterate the tags stored in the string. For each tag, the CODE reference in
C<$callback> is invoked once, being passed an extent object that represents
the extent of the tag. These extents will have the C<anchor_before> and
C<anchor_after> flags defined.

 $callback->( $extent, $tagname, $tagvalue )

Options passed in C<%opts> may include:

=over 4

=item start => INT

Start at the given position; defaults to 0.

=item end => INT

End after the given position; defaults to end of string. This option overrides
C<len>.

=item len => INT

End after the given length beyond the start position; defaults to end of
string. This option only applies if C<end> is not given.

=back

=cut

sub iter_extents
{
   my $self = shift;
   my ( $callback, %opts ) = @_;

   my $start = exists $opts{start} ? $opts{start} :
                                     0;

   my $end   = exists $opts{end} ? $opts{end} :
               exists $opts{len} ? $start + $opts{len} :
                                   $self->length;

   my $tags = $self->{tags};

   foreach my $t ( @$tags ) {
      my ( $ts, $te, $tn, $tv, $tf ) = @$t;

      next if $te < $start;
      last if $ts >= $end;

      $callback->( $self->_mkextent( $ts, $te, $tf ), $tn, $tv );
   }
}

=head2 $st->iter_tags( $callback, %opts )

Iterate the tags stored in the string. For each tag, the CODE reference in
C<$callback> is invoked once, being passed the start point and length of the
tag.

 $callback->( $start, $length, $tagname, $tagvalue )

Options passed in C<%opts> are the same as for C<iter_extents()>.

=cut

sub iter_tags
{
   my $self = shift;
   my ( $callback, %opts ) = @_;

   $self->iter_extents(
      sub {
         my ( $e, $tn, $tv ) = @_;
         $callback->( $e->start, $e->length, $tn, $tv );
      },
      %opts
   );
}

=head2 $st->iter_extents_nooverlap( $callback, %opts )

Iterate non-overlapping extents of tags stored in the string. The CODE
reference in C<$callback> is invoked for each extent in the string where no
tags change. The entire set of tags active in that extent is given to the
callback. Because the extent covers possibly-multiple tags, it will not define
the C<anchor_before> and C<anchor_after> flags.

 $callback->( $extent, %tags )

The callback will be invoked over the entire length of the string, including
any extents with no tags applied.

Options may be passed in C<%opts> to control the range of the string iterated
over, in the same way as the C<iter_extents()> method.

=cut

sub iter_extents_nooverlap
{
   my $self = shift;
   my ( $callback, %opts ) = @_;

   my $start = exists $opts{start} ? $opts{start} :
                                     0;

   my $end   = exists $opts{end} ? $opts{end} :
               exists $opts{len} ? $start + $opts{len} :
                                   $self->length;

   my $tags = $self->{tags};

   my @active; # ARRAY of [ $ts, $te, $tn, $tv ]
   my $pos = $start;

   foreach my $t ( @$tags ) {
      my ( $ts, $te, $tn, $tv ) = @$t;

      next if $te < $start;
      last if $ts >= $end;

      while( $pos < $ts ) {
         my %activetags;
         my %tagends;
         my $rangeend = $ts;

         foreach ( @active ) {
            my ( undef, $e, $n, $v ) = @$_;

            $e < $rangeend and $rangeend = $e;
            next if $tagends{$n} and $tagends{$n} < $e;

            $activetags{$n} = $v;
            $tagends{$n} = $e;
         }

         $callback->( $self->_mkextent( $pos, $rangeend, 0 ), %activetags );

         $pos = $rangeend;
         @active = grep { $_->[1] > $pos } @active;
      }

      push @active, [ $ts, $te, $tn, $tv ];
   }

   while( $pos < $end ) {
      my %activetags;
      my %tagends;
      my $rangeend = $end;

      foreach ( @active ) {
         my ( undef, $e, $n, $v ) = @$_;

         $e < $rangeend and $rangeend = $e;
         next if $tagends{$n} and $tagends{$n} < $e;

         $activetags{$n} = $v;
         $tagends{$n} = $e;
      }

      $callback->( $self->_mkextent( $pos, $rangeend, 0 ), %activetags );

      $pos = $rangeend;
      @active = grep { $_->[1] > $pos } @active;
   }
}

=head2 $st->iter_tags_nooverlap( $callback, %opts )

Iterate extents of the string using C<iter_extents_nooverlap()>, but passing
the start and length of each extent to the callback instead of the extent
object.

 $callback->( $start, $length, %tags )

Options may be passed in C<%opts> to control the range of the string iterated
over, in the same way as the C<iter_extents()> method.

=cut

sub iter_tags_nooverlap
{
   my $self = shift;
   my ( $callback, %opts ) = @_;

   $self->iter_extents_nooverlap(
      sub {
         my ( $e, %tags ) = @_;
         $callback->( $e->start, $e->length, %tags );
      },
      %opts
   );
}

=head2 $st->iter_substr_nooverlap( $callback, %opts )

Iterate extents of the string using C<iter_extents_nooverlap()>, but passing
the substring of data instead of the extent object.

 $callback->( $substr, %tags )

Options may be passed in C<%opts> to control the range of the string iterated
over, in the same way as the C<iter_extents()> method.

=cut

sub iter_substr_nooverlap
{
   my $self = shift;
   my ( $callback, %opts ) = @_;

   $self->iter_extents_nooverlap(
      sub {
         my ( $e, %tags ) = @_;
         $callback->( $e->substr, %tags );
      },
      %opts,
   );
}

=head2 @names = $st->tagnames

Returns the set of tag names used in the string, in no particular order.

=cut

sub tagnames
{
   my $self = shift;

   my $tags = $self->{tags};

   my %tags;
   foreach my $t ( @$tags ) {
      $tags{$t->[2]}++;
   }

   keys %tags;
}

=head2 $tags = $st->get_tags_at( $pos )

Returns a HASH reference of all the tag values active at the given position.

=cut

sub get_tags_at
{
   my $self = shift;
   my ( $pos ) = @_;

   my $tags = $self->{tags};

   my %tags;

   # TODO: turn this into a binary search
   foreach my $t ( @$tags ) {
      my ( $ts, $te, $tn, $tv ) = @$t;

      next if $pos < $ts;
      last if $pos >= $te;

      $tags{$tn} = $tv;
   }

   return \%tags;
}

=head2 $value = $st->get_tag_at( $pos, $name )

Returns the value of the named tag at the given position, or C<undef> if the
tag is not applied there.

=cut

sub get_tag_at
{
   my $self = shift;
   my ( $pos, $name ) = @_;

   my $tags = $self->{tags};

   my $value;

   foreach my $t ( @$tags ) {
      my ( $ts, $te, $tn, $tv ) = @$t;

      next if $pos < $ts;
      last if $pos >= $te;

      $value = $tv if $tn eq $name;
   }

   return $value;
}

=head2 $extent = $st->get_tag_extent( $pos, $name )

If the named tag applies to the given position, returns the extent of the tag
at that position. If it does not, C<undef> is returned. If an extent is
returned it will define the C<anchor_before> and C<anchor_after> flags.

=cut

sub get_tag_extent
{
   my $self = shift;
   my ( $pos, $name ) = @_;

   my $tags = $self->{tags};

   my ( $start, $end, $flags );

   foreach my $t ( @$tags ) {
      my ( $ts, $te, $tn, undef, $tf ) = @$t;

      next if $pos < $ts;
      last if $pos >= $te;

      next unless $tn eq $name;

      $start = $ts;
      $end   = $te;
      $flags = $tf;
   }

   if( defined $start ) {
      return $self->_mkextent( $start, $end, $flags );
   }
   else {
      return undef;
   }
}

=head2 $extent = $st->get_tag_missing_extent( $pos, $name )

If the named tag does not apply at the given position, returns the extent of
the string around that position that does not have the tag. If it does exist,
C<undef> is returned. If an extent is returned it will not define the
C<anchor_before> and C<anchor_after> flags, as these do not make sense for the
range in which a tag is absent.

=cut

sub get_tag_missing_extent
{
   my $self = shift;
   my ( $pos, $name ) = @_;

   my $tags = $self->{tags};

   my $start = 0;

   foreach my $t ( @$tags ) {
      my ( $ts, $te, $tn ) = @$t;

      next unless $tn eq $name;

      if( $ts <= $pos and $te > $pos ) {
         return undef;
      }

      if( $ts > $pos ) {
         return $self->_mkextent( $start, $ts, 0 );
      }

      $start = $te;
   }

   return $self->_mkextent( $start, $self->length, 0 );
}

=head2 $st->set_substr( $start, $len, $newstr )

Modifies a extent of the underlying plain string to that given. The extent of
tags in the string are adjusted to cope with the modified region, and the
adjustment in length.

Tags entirely before the replaced extent remain unchanged.

Tags entirely within the replaced extent are deleted.

Tags entirely after the replaced extent are moved by appropriate amount to
ensure they still apply to the same characters as before.

Tags that start before and end after the extent remain, and have their lengths
suitably adjusted.

Tags that span just the start or end of the extent, but not both, are
truncated, so as to remove the part of the tag applied on the modified extent
but preserving that applied outside.

If C<$newstr> is a C<String::Tagged> object, then its tags will be applied to
C<$st> as appropriate. Edge-anchored tags in C<$newstr> will not be extended
through C<$st>, though they will apply as edge-anchored if they now sit at the
edge of the new string.

=cut

sub set_substr
{
   my $self = shift;
   my ( $start, $len, $new ) = @_;

   my $limit = $self->length;

   $start = $limit if $start > $limit;
   $len = ( $limit - $start ) if $len > ( $limit - $start );

   CORE::substr( $self->{str}, $start, $len ) = $new;

   my $oldend = $start + $len;
   my $newend = $start + CORE::length( $new );

   my $delta = $newend - $oldend;
   # Positions after $oldend have now moved up $delta places

   my $tags = $self->{tags};

   my $i = 0;

   for( ; $i < @$tags; $i++ ) {
      # In this loop we'll handle tags that start before the deleted section

      my $t = $tags->[$i];
      my ( $ts, $te, undef, undef, $tf ) = @$t;

      last if $ts >= $start and not( $tf & FLAG_ANCHOR_BEFORE );

      # Two cases:
      # A: Tag spans entirely outside deleted section - stretch/compress it
      #     We may have to collapse it to nothing, so delete it
      # B: Tag starts before but ends within deleted section - truncate it
      # Plus a case we don't care about
      #    Tag starts and ends entirely before the deleted section - ignore it

      if( $te > $oldend or 
          ( $te == $oldend and $tf & FLAG_ANCHOR_AFTER ) ) {
         # Case A
         $t->[1] += $delta;

         if( $t->[0] == $t->[1] ) {
            splice @$tags, $i, 1, ();
            $i--;
            next;
         }
      }
      elsif( $te > $start ) {
         # Case B
         $t->[1] = $start;
      }
   }

   for( ; $i < @$tags; $i++ ) {
      my $t = $tags->[$i];
      my ( $ts, $te ) = @$t;

      # In this loop we'll handle tags that start within the deleted section
      last if $ts >= $oldend;

      # Two cases
      # C: Tag contained entirely within deleted section - delete it
      # D: Tag starts within but ends after the deleted section - truncate it

      if( $te <= $oldend ) {
         # Case C
         splice @$tags, $i, 1;
         $i--;
         next;
      }
      else {
         # Case D
         $t->[0] = $newend;
         $t->[1] += $delta;
      }
   }

   for( ; $i < @$tags; $i++ ) {
      my $t = $tags->[$i];
      my ( $ts, $te, undef, undef, $tf ) = @$t;

      # In this loop we'll handle tags that start after the deleted section

      # One case
      # E: Tag starts and ends after the deleted section - move it
      $t->[0] += $delta unless $tf & FLAG_ANCHOR_BEFORE;
      $t->[1] += $delta;

      # If we've not moved the start (because it was FLAG_ANCHOR_BEFORE), we
      # might now have an ordering constraint violation. Better fix it.
      local $b = $t;
      foreach my $new_i ( reverse 0 .. $i-1 ) {
         local $a = $tags->[$new_i];

         last if _cmp_tags() <= 0;

         splice @$tags, $new_i, 0, splice @$tags, $i, 1, ();

         last;
      }
   }

   if( eval { $new->isa( __PACKAGE__ ) } ) {
      my $atstart = $start == 0;
      my $atend   = $newend == $self->length;

      $new->iter_extents( sub {
         my ( $e, $tn, $tv ) = @_;
         $self->apply_tag(
            ( $atstart && $e->anchor_before ) ? -1 : $start + $e->start,
            ( $atend   && $e->anchor_after  ) ? -1 : $e->length,
            $tn, $tv );
      } );
   }

   $self->_assert_sorted;

   return $self;
}

=head2 $st->insert( $start, $newstr )

Insert the given string at the given position. A shortcut around
C<set_substr()>.

If C<$newstr> is a C<String::Tagged> object, then its tags will be applied to
C<$st> as appropriate. If C<$start> is 0, any before-anchored tags in will
become before-anchored in C<$st>.

=cut

sub insert
{
   my $self = shift;
   my ( $at, $new ) = @_;
   $self->set_substr( $at, 0, $new );
}

=head2 $st->append( $newstr )

=head2 $st .= $newstr

Append to the underlying plain string. A shortcut around C<set_substr()>.

If C<$newstr> is a C<String::Tagged> object, then its tags will be applied to
C<$st> as appropriate. Any after-anchored tags in will become after-anchored
in C<$st>.

=cut

use overload '.=' => 'append';

sub append
{
   my $self = shift;
   my ( $new ) = @_;

   $self->insert( $self->length, $new );
}

=head2 $st->append_tagged( $newstr, %tags )

Append to the underlying plain string, and apply the given tags to the
newly-inserted extent.

=cut

sub append_tagged
{
   my $self = shift;
   my ( $new, %tags ) = @_;

   my $start = $self->length;
   my $len   = CORE::length( $new );

   $self->append( $new );
   $self->apply_tag( $start, $len, $_, $tags{$_} ) for keys %tags;
}

=head2 $ret = $st->concat( $other )

=head2 $ret = $st . $other

Returns a new C<String::Tagged> containing the two strings concatenated
together, preserving any tags present. This method overloads normal string
concatenation operator, so expressions involving C<String::Tagged> values
retain their tags.

=cut

use overload '.' => 'concat';

sub concat
{
   my $self = shift;
   my ( $other, $swap ) = @_;

   my $ret = __PACKAGE__->new( $self );
   return $ret->insert( 0, $other ) if $swap;
   return $ret->append( $other );
}

=head2 $ret = $st->debug_sprintf

Returns a representation of the string data and all the tags, suitable for
debug printing or other similar use. This is a format such as is given in the
DESCRIPTION section above.

The output will consist of a number of lines, the first containing the plain
underlying string, then one line per tag. The line shows the extent of the tag
given by C<[---]> markers, or a C<|> in the special case of a tag covering
only a single character. Special markings of C<E<lt>> and C<E<gt>> indicate
tags which are "before" or "after" anchored.

For example:

  Hello, world
  [---]         word       => 1
 <[----------]> everywhere => 1
        |       space      => 1

=cut

sub debug_sprintf
{
   my $self = shift;

   my $str = $self->str;
   my $len = CORE::length( $str );

   my $maxnamelen = 0;

   my $ret = "  $str\n";

   $self->iter_tags( sub {
      my ( undef, undef, $name, undef ) = @_;
      CORE::length( $name ) > $maxnamelen and $maxnamelen = CORE::length( $name );
   } );

   foreach my $t ( @{ $self->{tags} } ) {
      my ( $ts, $te, $tn, $tv, $tf ) = @$t;

      $ret .= ( $tf & FLAG_ANCHOR_BEFORE ) ? " <" : "  ";

      $ret .= " " x $ts;

      my $tl = $te - $ts;

      if( $tl == 0 ) {
         $ret .= "";
      }
      elsif( $tl == 1 ) {
         $ret .= "|";
      }
      else {
         $ret .= "[" . ( "-" x ( $tl - 2 ) ) . "]";
      }

      $ret .= " " x ( $len - $te );

      $ret .= ( $tf & FLAG_ANCHOR_AFTER ) ? "> " : "  ";

      $ret .= sprintf "%-*s => %s\n", $maxnamelen, $tn, $tv;
   }

   return $ret;
}

=head1 Extent Objects

These objects represent a range of characters within the containing
C<String::Tagged> object. The range they represent is fixed at the time of
creation. If the containing string is modified by a call to C<set_substr()>
then the effect on the extent object is not defined. These objects should be
considered as relatively short-lived - used briefly for the purpose of
querying the result of an operation, then discarded soon after.

=cut

package # hide from CPAN indexer
  String::Tagged::Extent;

=head2 $extent->string

Returns the containing C<String::Tagged> object.

=cut

sub string
{
   shift->[0]
}

=head2 $extent->start

Returns the start index of the extent. This is the index of the first
character within the extent.

=cut

sub start
{
   shift->[1]
}

=head2 $extent->end

Returns the end index of the extent. This is the index of the first character
beyond the end of the extent.

=cut

sub end
{
   shift->[2]
}

=head2 $extent->anchor_before

True if this extent begins "before" the start of the string. Only certain
methods return extents with this flag defined.

=cut

sub anchor_before
{
   shift->[3] & String::Tagged::FLAG_ANCHOR_BEFORE;
}

=head2 $extent->anchor_after

True if this extent ends "after" the end of the string. Only certain methods
return extents with this flag defined.

=cut

sub anchor_after
{
   shift->[3] & String::Tagged::FLAG_ANCHOR_AFTER;
}

=head2 $extent->length

Returns the number of characters within the extent.

=cut

sub length
{
   my $self = shift;
   $self->end - $self->start;
}

=head2 $extent->substr

Returns the substring of the underlying plain string buffer contained by the
extent.

=cut

sub substr
{
   my $self = shift;
   $self->string->substr( $self->start, $self->length );
}

# Keep perl happy; keep Britain tidy
1;

__END__

=head1 TODO

=over 4

=item *

There are likely variations on the rules for C<set_substr()> that could
equally apply to some uses of tagged strings. Consider whether the behaviour
of modification is chosen per-method, per-tag, or per-string.

=back

=head1 AUTHOR

Paul Evans <leonerd@leonerd.org.uk>

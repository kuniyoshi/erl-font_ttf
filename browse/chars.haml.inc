        %h3 simbols, alphas, nums, kana
        %table
[% FOREACH cols IN chars.kana -%]
            %tr
[%   FOREACH col IN cols -%]
                %td(title="[% col %]") [% col %]
[%   END -%]
[% END -%]
        %h3 kanji1
        %table
[% FOREACH cols IN chars.kanji1 -%]
            %tr
[%   FOREACH col IN cols -%]
                %td(title="[% col %]") [% col %]
[%   END -%]
[% END -%]
        %h3 kanji2
        %table
[% FOREACH cols IN chars.kanji2 -%]
            %tr
[%   FOREACH col IN cols -%]
                %td(title="[% col %]") [% col %]
[%   END -%]
[% END -%]

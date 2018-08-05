/*
 * **********************************************
 * aux predicates (see ex5-aux.pl documentation)
 * **********************************************
 */
:- use_module('ex5-aux').

/*
 * **********************************************
 * Question 3:
 *
 * A relational database for Wikipedia management.
 *
 * The database contains the tables: page, namespaces,
 * category and categorylinks.
 * **********************************************
 */

% Signature: page_in_category(PageName, CategoryId)/2
% Purpose: Relation between a page name and a category id,
%          so that the page is included in the category.
%          and the category is not hidden.
% Examples:
% ?- page_in_category(cnn, X).
% X = 786983;
% X = 786983
%
% ?- page_in_category(X, 564677).
% X = ocpc;
% X = nbc.
%
% ?- PageInCategory(metropolitan, X).
% false.
%
page_in_category(PName, CatId) :-
	page(PId, _, PName, _),
	category(CatId, CatTitle, false),
	categorylinks(PId, CatTitle).

% Signature: splitter_category(CategoryId)/1
% Purpose: A category that has at least two pages.
%          Multiple right answers may be given.
%
% Examples:
% ?- splitter_category(689969).
% true.
%
% ?- splitter_category(564677).
% true.
%
% ?- SplitterCategory(858585).
% false.
%
splitter_category(CatId) :-
	category(CatId, CatTitle, _),
	categorylinks(X, CatTitle),
	categorylinks(Y, CatTitle),
	\=(X, Y).

% Signature: namespace_list(NamespaceName, PageList)/2
% Purpose: PageList includes all the pages in namespace NamespaceName.
%          The order of list elements is irrelevant.
% Examples:
% ?- namespace_list(article, X).
% X = [558585, 689695, 858585].
%
namespace_list(Name, PageList) :-
	namespaces(Ns_number, Name),
	listPages(Ns_number, [], PageList).

% Signature: listPages(Page_namespace, L)/2
% Purpose: L includes all PageIds with Page_namespace.
% Examples:
% ?- listPages(118, X).
% X = [464236].
listPages(Page_namespace, L) :- listPages(Page_namespace, [], L).

% Signature: listPages(Page_namespace, Acc, L)/3
% Purpose: sub function of listPages(Page_namespace, L).
listPages(Page_namespace, Acc, L) :-
	page(Page_id ,Page_namespace, _, _),
	not_member(Page_id, Acc),
        !,
        listPages(Page_namespace, [Page_id | Acc], L).
listPages(_, L, L).

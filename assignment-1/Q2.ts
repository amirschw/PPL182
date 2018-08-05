// Q2 TypeScript Programming

const assert = require('assert');
import {map, reduce, filter} from 'ramda'

// Q2.1 BinTree
interface BinTree {
    root: number;
    left?: BinTree;
    right?: BinTree;
};

// Q.2.1.1 TreePreArray
const TreePreArray = (t: BinTree): number[] => 
    t === undefined ? [] :
    [t.root].concat(TreePreArray(t.left)).concat(TreePreArray(t.right));

const testTreePreArray = (): void => {
    let t: BinTree = {root: 1};
    let expected: number[] = [1];
    assert.deepStrictEqual(TreePreArray(t), expected);

    t = {root: 1, left: {root: 2}, right: {root: 3}};
    expected = [1, 2, 3];
    assert.deepStrictEqual(TreePreArray(t), expected);

    t = {root: 1, left: {root: 2, right: {root: 3, left: {root: 4}}},
    right: {root: 5, left: {root: 6, right: {root: 7}}, right: {root: 8}}};
    expected = [1, 2, 3, 4, 5, 6, 7, 8];
    assert.deepStrictEqual(TreePreArray(t), expected);
}

testTreePreArray();

// Q.2.1.2 TreeInArray
const TreeInArray = (t: BinTree): number[] => 
    t === undefined ? [] :
    TreeInArray(t.left).concat([t.root]).concat(TreeInArray(t.right));

const testTreeInArray = (): void => {
    let t: BinTree = {root: 1};
    let expected: number[] = [1];
    assert.deepStrictEqual(TreeInArray(t), expected);

    t = {root: 2, left: {root: 1}, right: {root: 3}};
    expected = [1, 2, 3];
    assert.deepStrictEqual(TreeInArray(t), expected);

    t = {root: 4, left: {root: 1, right: {root: 3, left: {root: 2}}},
    right: {root: 7, left: {root: 5, right: {root: 6}}, right: {root: 8}}};
    expected = [1, 2, 3, 4, 5, 6, 7, 8];
    assert.deepStrictEqual(TreeInArray(t), expected);
}

testTreeInArray();

// Q2.1.3 TreePostArray
const TreePostArray = (t: BinTree): number[] => 
    t === undefined ? [] :
    TreePostArray(t.left).concat(TreePostArray(t.right)).concat([t.root]);

const testTreePostArray = (): void => {
    let t: BinTree = {root: 1};
    let expected: number[] = [1];
    assert.deepStrictEqual(TreePostArray(t), expected);

    t = {root: 3, left: {root: 1}, right: {root: 2}};
    expected = [1, 2, 3];
    assert.deepStrictEqual(TreePostArray(t), expected);

    t = {root: 8, left: {root: 3, right: {root: 2, left: {root: 1}}},
    right: {root: 7, left: {root: 5, right: {root: 4}}, right: {root: 6}}};
    expected = [1, 2, 3, 4, 5, 6, 7, 8];
    assert.deepStrictEqual(TreePostArray(t), expected);
}

testTreePostArray();

// GBinTree
interface GBinTree<T> {
    root: T;
    left?: GBinTree<T>;
    right?: GBinTree<T>;
};

// Q2.1.4 GBinTreePreArray
const GBinTreePreArray = <T> (t: GBinTree<T>): T[] => 
    t === undefined ? [] :
    [t.root].concat(GBinTreePreArray(t.left)).concat(GBinTreePreArray(t.right));

const testGBinTreePreArray = (): void => {
    let t: GBinTree<any> = {root: "a"};
    let expected: any[] = ["a"];
    assert.deepStrictEqual(GBinTreePreArray(t), expected);

    t = {root: "a", left: {root: 2}, right: {root: "c", right: {root: 4}}};
    expected = ["a", 2, "c", 4];
    assert.deepStrictEqual(GBinTreePreArray(t), expected);

    t = {root: "a", left: {root: 2, right: {root: "c", left: {root: 4}}},
    right: {root: "e", left: {root: false, right: {root: 7}}, right: {root: "h"}}};
    expected = ["a", 2, "c", 4, "e", false, 7, "h"];
    assert.deepStrictEqual(GBinTreePreArray(t), expected);
}

testGBinTreePreArray();

// Q2.1.5 GBinTreeInArray
const GBinTreeInArray = <T> (t: GBinTree<T>): T[] => 
    t === undefined ? [] :
    GBinTreeInArray(t.left).concat([t.root]).concat(GBinTreeInArray(t.right));

const testGBinTreeInArray = (): void => {
    let t: GBinTree<any> = {root: "a"};
    let expected: any[] = ["a"];
    assert.deepStrictEqual(GBinTreeInArray(t), expected);

    t = {root: 2, left: {root: "a"}, right: {root: "c"}};
    expected = ["a", 2, "c"];
    assert.deepStrictEqual(GBinTreeInArray(t), expected);

    t = {root: 4, left: {root: "a", right: {root: "c", left: {root: 2}}},
    right: {root: 7, left: {root: "e", right: {root: false}}, right: {root: "h"}}};
    expected = ["a", 2, "c", 4, "e", false, 7, "h"];
    assert.deepStrictEqual(GBinTreeInArray(t), expected);
}

testGBinTreeInArray();

// Q2.1.6 GBinTreePostArray
const GBinTreePostArray = <T> (t: GBinTree<T>): T[] =>
    t === undefined ? [] :
    GBinTreePostArray(t.left).concat(GBinTreePostArray(t.right)).concat([t.root]);

const testGBinTreePostArray = (): void => {
    let t: GBinTree<any> = {root: "a"};
    let expected: any[] = ["a"];
    assert.deepStrictEqual(GBinTreePostArray(t), expected);

    t = {root: "c", left: {root: "a"}, right: {root: 2}};
    expected = ["a", 2, "c"];
    assert.deepStrictEqual(GBinTreePostArray(t), expected);

    t = {root: "h", left: {root: "c", right: {root: 2, left: {root: "a"}}},
    right: {root: 7, left: {root: "e", right: {root: 4}}, right: {root: false}}};
    expected = ["a", 2, "c", 4, "e", false, 7, "h"];
    assert.deepStrictEqual(GBinTreePostArray(t), expected);
}

testGBinTreePostArray();


// Q2.2 Subsets

// Q2.2.1 KSubsets
const KSubsets = <T> (A: T[], k: number): T[][] => 
    k === 0 ? [[]] :
    A.length === k ? [A] :
    A.length < k || k < 0 ? [] :
    map((x: T[]) => [A[0]].concat(x), KSubsets(A.slice(1), k-1)).concat(KSubsets(A.slice(1), k));

const testKSubsets = (): void => {
    let A: number[] = [1, 2, 3];
    let expectedA: number[][] = [[]];
    assert.deepStrictEqual(KSubsets(A, 0).sort(), expectedA.sort());

    expectedA = [[1], [2], [3]];
    assert.deepStrictEqual(KSubsets(A, 1).sort(), expectedA.sort());

    expectedA = [[1, 2], [1, 3], [2, 3]];
    assert.deepStrictEqual(KSubsets(A, 2).sort(), expectedA.sort());

    expectedA = [[1, 2, 3]];
    assert.deepStrictEqual(KSubsets(A, 3).sort(), expectedA.sort());

    let B: any[] = ["a", 2, true, [{a: 1}]];
    let expectedB: any[][] = [["a", 2], ["a", true], ["a", [{a: 1}]], [2, true], [2, [{a: 1}]], [true, [{a: 1}]]];
    assert.deepStrictEqual(KSubsets(B, 2).sort(), expectedB.sort());
}

testKSubsets();

// Q2.2.2 AllSubsets
const AllSubsets = <T> (A: T[]): T[][] => 
    A.length === 0 ? [[]] :
    map((x: T[]) => [A[0]].concat(x), AllSubsets(A.slice(1))).concat(AllSubsets(A.slice(1)));

const testAllSubsets = (): void => {
    let A: number[] = [];
    let expectedA: number[][] = [[]];
    assert.deepStrictEqual(AllSubsets(A).sort(), expectedA.sort());

    A = [1, 2, 3];
    expectedA = [[], [1], [2], [3], [1, 2], [1, 3], [2, 3], [1, 2, 3]];
    assert.deepStrictEqual(AllSubsets(A).sort(), expectedA.sort());

    let B: any[] = ["a", 2, true, [{a: 1}]];
    let expectedB: any[][] = [[], ["a"], [2], [true], [[{a: 1}]], ["a", 2], ["a", true], ["a", [{a: 1}]],
                              [2, true], [2, [{a: 1}]], [true, [{a: 1}]], ["a", 2, true], ["a", 2, [{a: 1}]],
                              ["a", true, [{a: 1}]], [2, true, [{a: 1}]], ["a", 2, true, [{a: 1}]]];
    assert.deepStrictEqual(AllSubsets(B).sort(), expectedB.sort());
}

testAllSubsets();


// Q2.3 Flatmap

// Q2.3.1 Flatmap Definition
const flatmap = <T1, T2> (f: (x: T1) => T2[], A: T1[]): T2[] =>
    reduce((acc: T2[], curr: T2[]) => acc.concat(curr), [], map(f, A));

const testFlatmap = (): void => {
    let f = (x: number[][]): number[] => x[0];
    let A: number[][][] = [[[1,2], [3,4]], [[5,6], [7,8]]];
    let expectedA: number[] = [1, 2, 5, 6];
    assert.deepStrictEqual(flatmap(f, A), expectedA);

    f = (x) => [0];
    expectedA = [0, 0];
    assert.deepStrictEqual(flatmap(f, A), expectedA);

    let g = (x: string): Boolean[] => x === "a" || x === "f" ? [true] : [false];
    let B: string[] = ["a", "b", "c", "d", "e", "f"];
    let expectedB: Boolean[] = [true, false, false, false, false, true];
    assert.deepStrictEqual(flatmap(g, B), expectedB);
}

testFlatmap();

// Q2.3.2 Using Flatmap
interface Boxart {
    width: number;
    height: number;
    url: string
};

interface Video {
    id: number;
    title: string;
    boxarts: Boxart[];
    url: string;
    rating: number;
    bookmark: {id: number, time: number}[]
};

interface MovieList {
    name: string;
    videos: Video[]
};

const getBoxartUrl = (boxarts: Boxart[]): string =>
    reduce((acc: string, curr: Boxart) => acc.concat(curr.url), "",
            filter((box: Boxart) => box.width === 150 && box.height === 200, boxarts));

const getBoxartsHelper = (l: MovieList): {id: number, title: string, boxart: string}[] =>
    map((v: Video) => ({id: v.id, title: v.title, boxart: getBoxartUrl(v.boxarts)}), l.videos);

const getBoxarts = (A: MovieList[]): {id: number, title: string, boxart: string}[] =>
    flatmap(getBoxartsHelper, A);

const testGetBoxarts = (): void => {
    let movieLists: MovieList[] = [];
    let expected: {id: number, title: string, boxart: string}[] = [];
    assert.deepStrictEqual(getBoxarts(movieLists).sort(), expected.sort());

    movieLists = [
        {
            name: "Instant Queue",
            videos: [
                {
                    "id": 70111470,
                    "title": "Die Hard",
                    "boxarts": [
                        { width: 150, height: 200, url: "http://cdn-0.nflximg.com/images/2891/DieHard150.jpg" },
                        { width: 200, height: 200, url: "http://cdn-0.nflximg.com/images/2891/DieHard200.jpg" }
                    ],
                    "url": "http://api.netflix.com/catalog/titles/movies/70111470",
                    "rating": 4.0,
                    "bookmark": []
                }
            ]
        },
        {
            name: "New Releases",
            videos: []
        }
    ];
    expected = [{id: 70111470, title: 'Die Hard', boxart: 'http://cdn-0.nflximg.com/images/2891/DieHard150.jpg'}];
    assert.deepStrictEqual(getBoxarts(movieLists).sort(), expected.sort());

    movieLists = [
        {
            name: "Instant Queue",
            videos: [
                {
                    "id": 70111470,
                    "title": "Die Hard",
                    "boxarts": [
                        { width: 150, height: 200, url: "http://cdn-0.nflximg.com/images/2891/DieHard150.jpg" },
                        { width: 200, height: 200, url: "http://cdn-0.nflximg.com/images/2891/DieHard200.jpg" }
                    ],
                    "url": "http://api.netflix.com/catalog/titles/movies/70111470",
                    "rating": 4.0,
                    "bookmark": []
                },
                {
                    "id": 654356453,
                    "title": "Bad Boys",
                    "boxarts": [
                        { width: 200, height: 200, url: "http://cdn-0.nflximg.com/images/2891/BadBoys200.jpg" },
                        { width: 150, height: 200, url: "http://cdn-0.nflximg.com/images/2891/BadBoys150.jpg" }
    
                    ],
                    "url": "http://api.netflix.com/catalog/titles/movies/70111470",
                    "rating": 5.0,
                    "bookmark": [{ id: 432534, time: 65876586 }]
                }
            ]
        },
        {
            name: "New Releases",
            videos: [
                {
                    "id": 65432445,
                    "title": "The Chamber",
                    "boxarts": [
                        { width: 150, height: 200, url: "http://cdn-0.nflximg.com/images/2891/TheChamber150.jpg" },
                        { width: 200, height: 200, url: "http://cdn-0.nflximg.com/images/2891/TheChamber200.jpg" }
                    ],
                    "url": "http://api.netflix.com/catalog/titles/movies/70111470",
                    "rating": 4.0,
                    "bookmark": []
                },
                {
                    "id": 675465,
                    "title": "Fracture",
                    "boxarts": [
                        { width: 200, height: 200, url: "http://cdn-0.nflximg.com/images/2891/Fracture200.jpg" },
                        { width: 150, height: 200, url: "http://cdn-0.nflximg.com/images/2891/Fracture150.jpg" },
                        { width: 300, height: 200, url: "http://cdn-0.nflximg.com/images/2891/Fracture300.jpg" }
                    ],
                    "url": "http://api.netflix.com/catalog/titles/movies/70111470",
                    "rating": 5.0,
                    "bookmark": [{ id: 432534, time: 65876586 }]
                }
            ]
        }
    ];
    expected = [
        { id: 70111470,
          title: 'Die Hard',
          boxart: 'http://cdn-0.nflximg.com/images/2891/DieHard150.jpg' },
        { id: 654356453,
          title: 'Bad Boys',
          boxart: 'http://cdn-0.nflximg.com/images/2891/BadBoys150.jpg' },
        { id: 65432445,
          title: 'The Chamber',
          boxart: 'http://cdn-0.nflximg.com/images/2891/TheChamber150.jpg' },
        { id: 675465,
          title: 'Fracture',
          boxart: 'http://cdn-0.nflximg.com/images/2891/Fracture150.jpg' }
    ];
    assert.deepStrictEqual(getBoxarts(movieLists).sort(), expected.sort());
}

testGetBoxarts();

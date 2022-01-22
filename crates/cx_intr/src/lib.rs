use std::collections::hash_map::RandomState;
use std::collections::{BTreeMap, HashMap};
use std::hash::{BuildHasher, Hash, Hasher};
use std::marker::PhantomData;
use std::ops::{AddAssign, Index, SubAssign};

pub trait InternerToken: Copy + Clone + Eq + PartialEq + Ord + PartialOrd {}

/// # Safety
/// [`Self::generate_token`] must always return a token it didn't return before,
/// except when [`IteratorTokenGeneratorUndo::undo`] is called.
pub unsafe trait InternerTokenGenerator<V, T>
where
    T: InternerToken,
{
    fn new() -> Self;
    fn generate_token(&mut self, element: &V) -> T;
}

pub trait InternerTokenGeneratorUndo<V, T>: InternerTokenGenerator<V, T>
where
    T: InternerToken,
{
    fn undo(&mut self);
}

pub struct SeqGenerator<T: InternerToken> {
    next: T,
}

pub trait One {
    const ONE: Self;
}

macro_rules! num_interner_token {
    ($t:ty) => {
        impl InternerToken for $t {}

        impl One for $t {
            const ONE: Self = 1;
        }
    };
}

num_interner_token!(i8);
num_interner_token!(u8);
num_interner_token!(i16);
num_interner_token!(u16);
num_interner_token!(i32);
num_interner_token!(u32);
num_interner_token!(i64);
num_interner_token!(u64);
num_interner_token!(isize);
num_interner_token!(usize);

unsafe impl<V, T> InternerTokenGenerator<V, T> for SeqGenerator<T>
where
    T: InternerToken + Default + AddAssign + One,
{
    fn new() -> Self {
        Self {
            next: Default::default(),
        }
    }

    fn generate_token(&mut self, _: &V) -> T {
        let next = self.next;
        self.next += T::ONE;
        next
    }
}

impl<V, T> InternerTokenGeneratorUndo<V, T> for SeqGenerator<T>
where
    T: InternerToken + Default + AddAssign + SubAssign + One,
{
    fn undo(&mut self) {
        self.next -= T::ONE;
    }
}

#[deprecated(
    since = "0.1.0",
    note = "may be prone to hash collisions and therefore is not recommended"
)]
pub struct HasherGenerator {}

unsafe impl<V> InternerTokenGenerator<V, u64> for HasherGenerator
where
    V: Hash,
{
    fn new() -> Self {
        Self {}
    }

    fn generate_token(&mut self, element: &V) -> u64 {
        let s = RandomState::new();
        let mut hasher = s.build_hasher();
        Hash::hash(element, &mut hasher);

        hasher.finish()
    }
}

pub trait InternerStorage<V, T>
where
    T: InternerToken,
{
    fn new() -> Self;
    fn new_with_capacity(capacity: usize) -> Self;

    fn insert(&mut self, value: V) -> T;
    fn get(&self, token: T) -> Option<&V>;
}

/// A storage backed by a BTreeMap.
/// Insert: O(log n)
/// Get: O(n)
pub struct BTreeMapInternerStorage<V, T, G = SeqGenerator<T>>
where
    V: Ord,
    T: InternerToken,
    G: InternerTokenGenerator<V, T>,
{
    map: BTreeMap<V, T>,
    gen: G,
}

impl<V, T, G> InternerStorage<V, T> for BTreeMapInternerStorage<V, T, G>
where
    V: Ord,
    T: InternerToken,
    G: InternerTokenGenerator<V, T>,
{
    fn new() -> Self {
        Self {
            map: BTreeMap::new(),
            gen: G::new(),
        }
    }

    fn new_with_capacity(_capacity: usize) -> Self {
        Self {
            map: BTreeMap::new(),
            gen: G::new(),
        }
    }

    fn insert(&mut self, value: V) -> T {
        use std::collections::btree_map::Entry;

        match self.map.entry(value) {
            Entry::Vacant(v) => {
                let value = v.into_key();
                let t = self.gen.generate_token(&value);
                self.map.insert(value, t);
                t
            }
            Entry::Occupied(o) => *o.get(),
        }
    }

    fn get(&self, token: T) -> Option<&V> {
        self.map.iter().find(|(_, t)| **t == token).map(|(v, _)| v)
    }
}

/// A storage backed by a Vec.
/// Insert: O(n)
/// Get: O(1)
///
/// A downside of this storage is that you can't use it with a custom [`InternerTokenGenerator`].
pub struct VecInternerStorage<V>
where
    V: Eq,
{
    vec: Vec<V>,
    gen: SeqGenerator<usize>,
}

impl<V> InternerStorage<V, usize> for VecInternerStorage<V>
where
    V: Eq,
    SeqGenerator<usize>: InternerTokenGenerator<V, usize>,
{
    fn new() -> Self {
        Self {
            vec: Vec::new(),
            gen: SeqGenerator::new(),
        }
    }

    fn new_with_capacity(capacity: usize) -> Self {
        Self {
            vec: Vec::with_capacity(capacity),
            gen: SeqGenerator::new(),
        }
    }

    fn insert(&mut self, value: V) -> usize {
        if let Some(p) = self.vec.iter().position(|v| *v == value) {
            return p;
        }

        let index = self.gen.generate_token(&value);
        debug_assert_eq!(
            index,
            self.vec.len(),
            "Index should be the same as the length of the vector when inserting"
        );
        self.vec.push(value);
        index
    }

    fn get(&self, token: usize) -> Option<&V> {
        self.vec.get(token)
    }
}

impl<V> From<BTreeMapInternerStorage<V, usize, SeqGenerator<usize>>> for VecInternerStorage<V>
where
    V: Eq + Ord,
{
    fn from(t: BTreeMapInternerStorage<V, usize, SeqGenerator<usize>>) -> Self {
        let len = t.map.len();
        let mut vec = Vec::<V>::with_capacity(len);
        #[cfg(debug_assertions)]
        let mut check = vec![false; len];

        // SAFETY: BTreeMapInternerStorage only used the SeqGenerator,
        // and therefore it will have the entire 0..len() token range,
        // with no duplicates.
        unsafe {
            let ptr = vec.as_mut_ptr();
            for (v, p) in t.map.into_iter() {
                debug_assert!(
                    p < len,
                    "Token {p} should have been less than the length of the vector {len}",
                );
                #[cfg(debug_assertions)]
                {
                    debug_assert!(!check[p], "Token {p} should not have been set yet");
                    check[p] = true;
                }
                ptr.add(p).write(v);
            }
            vec.set_len(len);
        }

        Self { vec, gen: t.gen }
    }
}

pub struct HashMapInternerStorage<V, T, G = SeqGenerator<T>>
where
    V: Eq + Hash,
    T: InternerToken,
    G: InternerTokenGenerator<V, T>,
{
    map: HashMap<V, T>,
    gen: G,
}

impl<V, T, G> InternerStorage<V, T> for HashMapInternerStorage<V, T, G>
where
    V: Eq + Hash,
    T: InternerToken,
    G: InternerTokenGenerator<V, T>,
{
    fn new() -> Self {
        Self {
            map: HashMap::new(),
            gen: G::new(),
        }
    }

    fn new_with_capacity(capacity: usize) -> Self {
        Self {
            map: HashMap::with_capacity(capacity),
            gen: G::new(),
        }
    }

    fn insert(&mut self, value: V) -> T {
        use std::collections::hash_map::Entry;

        match self.map.entry(value) {
            Entry::Vacant(v) => {
                let value = v.into_key();
                let t = self.gen.generate_token(&value);
                self.map.insert(value, t);
                t
            }
            Entry::Occupied(o) => *o.get(),
        }
    }

    fn get(&self, token: T) -> Option<&V> {
        self.map.iter().find(|(_, t)| **t == token).map(|(v, _)| v)
    }
}

pub struct Interner<V, T = usize, S = VecInternerStorage<V>>
where
    T: InternerToken,
    S: InternerStorage<V, T>,
{
    storage: S,

    _p: PhantomData<(V, T)>,
}

impl<V, T, S> Interner<V, T, S>
where
    T: InternerToken,
    S: InternerStorage<V, T>,
{
    pub fn new() -> Self {
        Self {
            storage: S::new(),
            _p: PhantomData,
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            storage: S::new_with_capacity(capacity),
            _p: PhantomData,
        }
    }

    pub fn intern(&mut self, value: V) -> T {
        self.storage.insert(value)
    }

    pub fn get(&self, token: T) -> Option<&V> {
        self.storage.get(token)
    }

    pub fn map_storage<S2>(self) -> Interner<V, T, S2>
    where
        S2: From<S> + InternerStorage<V, T>,
    {
        Interner {
            storage: S2::from(self.storage),
            _p: PhantomData,
        }
    }
}

impl<V, T, S> Index<T> for Interner<V, T, S>
where
    T: InternerToken,
    S: InternerStorage<V, T>,
{
    type Output = V;

    fn index(&self, index: T) -> &Self::Output {
        self.get(index).expect("Index missing from interner")
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use crate::{
        BTreeMapInternerStorage, HashMapInternerStorage, HasherGenerator, Interner,
        InternerStorage, InternerToken, SeqGenerator, VecInternerStorage,
    };

    #[test]
    fn test_interner() {
        fn test<T: InternerToken + Eq + Debug, S: InternerStorage<&'static str, T>>() {
            let mut interner = Interner::<_, _, S>::new();

            let a = interner.intern("a");
            let b = interner.intern("b");
            let c = interner.intern("c");
            let d = interner.intern("a");

            assert_eq!(interner[a], "a");
            assert_eq!(interner[b], "b");
            assert_eq!(interner[c], "c");
            assert_eq!(interner[d], "a");
            assert_eq!(a, d);
        }

        test::<usize, VecInternerStorage<&'static str>>();
        test::<usize, BTreeMapInternerStorage<&'static str, usize, SeqGenerator<usize>>>();
        test::<usize, HashMapInternerStorage<&'static str, usize, SeqGenerator<usize>>>();
        test::<u64, HashMapInternerStorage<&'static str, u64, HasherGenerator>>();
    }

    #[test]
    fn btreemap_to_vec() {
        let mut interner = Interner::<
            _,
            _,
            BTreeMapInternerStorage<&'static str, usize, SeqGenerator<usize>>,
        >::new();

        let a = interner.intern("a");
        let b = interner.intern("b");
        let c = interner.intern("c");
        let d = interner.intern("a");

        let interner = interner.map_storage::<VecInternerStorage<&'static str>>();

        assert_eq!(interner[a], "a");
        assert_eq!(interner[b], "b");
        assert_eq!(interner[c], "c");
        assert_eq!(interner[d], "a");
        assert_eq!(a, d);
    }
}

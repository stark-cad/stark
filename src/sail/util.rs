// STARK, a system for computer augmented design.

// SPDX-FileCopyrightText: © 2026 Matthew Rothlisberger
// SPDX-License-Identifier: AGPL-3.0-only

// STARK is licensed under the terms of the GNU Affero General Public
// License version 3. See the top-level LICENSES directory for the
// license text.

// Find full copyright information in the top-level COPYRIGHT file.

// <>

// src/sail/util.rs

// General-purpose Sail utilities. Object handling. Components for
// runtime systems. Types to improve ergonomics.

// <>

// TODO: generic 'safe handle' type and lock trait?

use std::ops;
use std::ptr;

use std::intrinsics::AtomicOrdering as AO;

// TODO: find a better / clearer name for this than Hdl

/// Handle for persistent and unmoving runtime structures
#[repr(transparent)]
pub struct Hdl<T> {
    inner: ptr::NonNull<T>,
}

impl<T> ops::Receiver for Hdl<T> {
    type Target = T;
}

unsafe impl<T> Send for Hdl<T> {}

impl<T> Clone for Hdl<T> {
    fn clone(&self) -> Self {
        Self { inner: self.inner }
    }
}

impl<T> Copy for Hdl<T> {}

impl<T> Hdl<T> {
    pub fn new(raw: *mut T) -> Self {
        Self {
            inner: ptr::NonNull::new(raw).unwrap(),
        }
    }

    pub fn raw(self) -> *mut T {
        self.inner.as_ptr()
    }
}

// TODO: autorelease or panic if dropped?
pub struct LockToken<T>(*mut u8, std::marker::PhantomData<T>);

#[inline(always)]
pub(crate) fn take_lock_at(lock: *mut u8) -> LockToken<()> {
    unsafe {
        while !std::intrinsics::atomic_cxchg::<_, { AO::AcqRel }, { AO::Acquire }>(
            lock,
            false as u8,
            true as u8,
        )
        .1
        {
            std::hint::spin_loop();
        }
    }
    LockToken(lock, std::marker::PhantomData)
}

#[inline(always)]
pub(crate) fn release_lock_at(token: LockToken<()>) {
    let lock = token.0;
    unsafe {
        std::intrinsics::atomic_store::<_, { AO::Release }, false>(lock, false as u8);
    }
}

#[inline(always)]
pub(crate) fn locked_p_at(lock: *const u8) -> bool {
    unsafe { std::intrinsics::atomic_load::<_, { AO::Acquire }, false>(lock) != false as u8 }
}

pub trait Lock
where
    Self: Sized,
{
    fn lock_pos(self: Hdl<Self>) -> *mut u8;

    #[must_use]
    fn take_lock(self: Hdl<Self>) -> LockToken<Self> {
        let lock = self.lock_pos();
        LockToken(take_lock_at(lock).0, std::marker::PhantomData)
    }

    #[must_use]
    fn take_lock_nb(self: Hdl<Self>) -> Option<LockToken<Self>> {
        let lock = self.lock_pos();
        unsafe {
            if std::intrinsics::atomic_cxchg::<_, { AO::AcqRel }, { AO::Acquire }>(
                lock,
                false as u8,
                true as u8,
            )
            .1
            {
                Some(LockToken(lock, std::marker::PhantomData))
            } else {
                None
            }
        }
    }

    fn release_lock(self: Hdl<Self>, token: LockToken<Self>) {
        assert_eq!(token.0, self.lock_pos());
        release_lock_at(LockToken(token.0, std::marker::PhantomData))
    }

    fn locked_p(self: Hdl<Self>) -> bool {
        let lock = self.lock_pos();
        locked_p_at(lock)
    }
}

/// Raw vector handle
#[repr(transparent)]
pub struct RVH<T>(pub *mut RawVec<T>);

impl<T> Clone for RVH<T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<T> Copy for RVH<T> {}

impl<T> ops::Receiver for RVH<T> {
    type Target = RawVec<T>;
}

/// All ops raw
pub struct RawVec<T> {
    ptr: *mut T,
    len: usize,
    cap: usize,
}

impl<T> RawVec<T> {
    /// Creates a new heap-allocated vector with capacity
    pub fn new(cap: usize) -> Self {
        let init_layout = std::alloc::Layout::from_size_align(
            cap * std::mem::size_of::<T>(),
            std::mem::align_of::<T>(),
        )
        .unwrap();

        let ptr = unsafe { std::alloc::alloc(init_layout) as *mut T };

        Self { ptr, len: 0, cap }
    }

    /// Returns vector length
    pub fn len(self: RVH<T>) -> usize {
        unsafe { (*self.0).len }
    }

    /// Gives vector new capacity, possibly moving memory
    fn resize(self: RVH<T>, ncap: usize) {
        unsafe {
            let (len, ocap) = ((*self.0).len, (*self.0).cap);
            assert!(len < ncap);
            if ocap == ncap {
                return;
            }

            let new_layout = std::alloc::Layout::from_size_align(
                ncap * std::mem::size_of::<T>(),
                std::mem::align_of::<T>(),
            )
            .unwrap();

            let optr = (*self.0).ptr;
            let nptr = std::alloc::alloc(new_layout) as *mut T;

            std::ptr::copy_nonoverlapping(optr, nptr, (*self.0).len);

            (*self.0).ptr = nptr;

            let old_layout = std::alloc::Layout::from_size_align(
                ocap * std::mem::size_of::<T>(),
                std::mem::align_of::<T>(),
            )
            .unwrap();

            std::alloc::dealloc(optr as *mut _, old_layout);
        }
    }

    /// Adds value to the end of the vector
    pub fn push(self: RVH<T>, item: T) {
        unsafe {
            let len = (*self.0).len;
            if len == (*self.0).cap {
                self.resize(len * 2);
            }

            ptr::write((*(&raw mut (*self.0).ptr)).add(len), item);
            (*self.0).len += 1;
        }
    }

    /// Returns pointer to value at offset i
    pub fn index(self: RVH<T>, i: usize) -> *mut T {
        unsafe { (*self.0).ptr.add(i) }
    }
}

impl<T> Drop for RawVec<T> {
    fn drop(&mut self) {
        let layout = std::alloc::Layout::from_size_align(
            self.cap * std::mem::size_of::<T>(),
            std::mem::align_of::<T>(),
        )
        .unwrap();

        // Drop contents
        for i in 0..self.len {
            unsafe { std::ptr::drop_in_place(self.ptr.add(i)) }
        }

        unsafe {
            std::alloc::dealloc(self.ptr as *mut _, layout);
        }
    }
}

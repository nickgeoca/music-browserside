{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE GADTs  #-}

-- This data structure has the graphic/midi data stored into a list. 
-- The tree is integrated into the list.
--  Each element can be a 'list node', 'tree node', or 'nil'
--  See fmap to get sense of operation
-- Purpose is to be able to click on HTML canvas and start playing from there.
--  So the tree is searched for coordinates, then midi data is accessed as a list.
--  Caveat is to store the state of the midi throughout the list. (Volume may change half way through, but if started playing 3/4 through, then how to know?)
--   The strategy is to store the state every so often in the 'tree nodes'. When searching the tree and found the location, the 'tree node' will have the state.

----------------------------------------------------------------------------------------------------                      
-- List-Tree Data Structure 
----------------------------------------------------------------------------------------------------

data BinaryTree a = Branch {
  tL :: Maybe (BinaryTree a),
  tR :: Maybe (BinaryTree a),
  tD :: a  -- 'a' is at end, so it makes printing easier in the application.
  } deriving (Show, Read)

data LTNode k ds d = LTNode {
  ltKey      :: k,
  ltNodeDS   :: ds,
  ltNodeData :: d
  } deriving (Show, Read)

data ListTree key datum where
  LTTree     :: BinaryTree (LTNode key (ListTree key datum) datum) -> ListTree key datum
  LTList     :: datum  -> ListTree key datum -> ListTree key datum
  LTNil      :: ListTree key datum
  deriving (Show, Read)


listToLT :: [a] -> ListTree key a
listToLT []     = LTNil
listToLT (x:xs) = LTList x (listToLT xs)

getLTDataRestMaybe ds = 
  case ds of
   LTNil                                -> Nothing
   LTList d ds'                         -> Just (d,ds') 
   LTTree (Branch _ _ (LTNode _ ds' d)) -> Just (d,ds')

-- fmap. See fmap2 for alternative version. 
-- Use this for functor (higher kinded version)
--  * Ex: instance Functor (ListTree ExampleType) where fmap f lt = fmapK2 f lt
--  * Functor doesn't work with higher kinded type
fmapK2 :: (Ord keyT) => 
          (datTOld -> datTNew) ->   -- fn Datum
          ListTree keyT datTOld ->  -- ds
          ListTree keyT datTNew     -- ds
fmapK2 f lt = ltConnectTree $   -- 2) Connect tree
              map f lt          -- 1) Tree becomes unconnected
  where map _ LTNil         = LTNil   
        map f (LTList d ds) = LTList (f d) (map f ds)
        map f (LTTree (Branch l       r        (LTNode k  ds         d)))
            = LTTree $ Branch Nothing Nothing $ LTNode k (map f ds) (f d)

-- Map over data structure with two functions: fnData, fnKey 
fmap2 :: (Ord keyTOld, Ord keyTNew) => 
         (datTOld -> datTNew) ->      -- fn Datum
         (keyTOld -> keyTNew) ->      -- fn Key
         ListTree keyTOld datTOld ->  -- ds
         ListTree keyTNew datTNew     -- ds
fmap2 fnData fnKey lt = ltConnectTree $     -- 2) Connect Tree
                        map fnData fnKey lt -- 1) Tree becomes unconnected
  where map _ _ LTNil   = LTNil
        map fnData fnKey (LTList   d          ds) 
                        = LTList (fnData d) (map fnData fnKey ds)
        map fnData fnKey (LTTree  (Branch l       r        (LTNode  k         ds                    d)))
                        = LTTree $ Branch Nothing Nothing $ LTNode (fnKey k) (map fnData fnKey ds) (fnData d)

{-
-- CONTINUE
-- TODO: make search tree based instead of list search
-- Searches the listTree using the tree. Returns ...
search2 :: keySearch -> ListTree key datum -> Maybe (ltState, ListTree key datum)
search2 key lt = search' key ds (MidiContext 127 60)  -- TODO: Make a default midicontext here. Or some alterantive method
-}

search :: (Int,Int) -> MidiHgltDS -> Maybe (MidiContext, MidiHgltDS)
search key ds = search' key ds (MidiContext 127 60)  -- TODO: Make a default midicontext here. Or some alterantive method


-- TODO: Need to pick up the local contexts in this function
search' :: (Int,Int) -> MidiHgltDS -> MidiContext -> Maybe (MidiContext, MidiHgltDS)
search' key (LTTree (Branch _ _(LTNode h ds d))) ctxt = let ctxt' = fromMaybe ctxt (getMidiContext d) 
                                                        in if pointInBox key h  
                                                           then Just (ctxt', ds)
                                                           else search' key ds ctxt'
search' key (LTList d ds) ctxt = case getHgltBorder d of
                                  Just h  -> if pointInBox key h  
                                             then Just (ctxt, ds)
                                             else search' key ds ctxt
                                  Nothing ->      search' key ds ctxt
search' _ LTNil _ = Nothing


ltConnectTree :: (Ord keyT) =>
                  ListTree keyT elmT -> 
                  ListTree keyT elmT
ltConnectTree lt = lt -- TODO: This is unfinished

{-
ltConnectTree :: (Ord keyT) =>
                  ListTree keyT elmT -> 
                  ListTree keyT elmT
ltConnectTree lt = 


BinaryTree* sortedListToBST(ListNode *& list, int start, int end) {
  if (start > end) return NULL;
  // same as (start+end)/2, avoids overflow
  int mid = start + (end - start) / 2;
  BinaryTree *parent = new BinaryTree(list->data);
  BinaryTree *leftChild = sortedListToBST(list, start, mid-1);
  parent->left = leftChild;
  list = list->next;
  parent->right = sortedListToBST(list, mid+1, end);
  return parent;
}
 
BinaryTree* sortedListToBST(ListNode *head, int n) {
  return sortedListToBST(head, 0, n-1);
}
-}

{-
-- NOTE: Should this be strict?
instance Foldable (ListTree HgltBorder) where
  foldMap f LTNil         = mempty
  foldMap f (LTList d ds) = f d `mappend` foldMap f ds
  foldMap f (LTTree (Branch l r (LTNode k ds d)))
                          = f d `mappend` foldMap f ds
  foldr f b = go
    where go LTNil         = b
          go (LTList d ds) = d `f` go ds 
          go (LTTree (Branch l r (LTNode k ds d)))
                           = d 'f' go ds 
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Graphics.Rendering.OpenGL.Lens where

import Control.Lens
import Control.Applicative
import GHC.Ptr (Ptr)

import Graphics.Rendering.OpenGL.GL (GLint, GLsizei)
import Graphics.Rendering.OpenGL.GL.CoordTrans (Position(..), Size(..), Plane(..))
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferObjects (RenderbufferSize(..))
import Graphics.Rendering.OpenGL.GL.Tensor (
    Vector1(..), Vector2(..), Vector3(..), Vector4(..),
    Vertex1(..), Vertex2(..), Vertex3(..), Vertex4(..))
import Graphics.Rendering.OpenGL.GL.Texturing.Specification (
    TexturePosition1D(..), TexturePosition2D(..), TexturePosition3D(..),
    TextureSize1D(..), TextureSize2D(..), TextureSize3D(..))
import Graphics.Rendering.OpenGL.GL.VertexAttributes (
    TexCoord1(..), TexCoord2(..), TexCoord3(..), TexCoord4(..),
    Color3(..), Color4(..),
    FogCoord1(..), Index1(..), Normal3(..))
import Graphics.Rendering.OpenGL.GL.VertexArrays (
    VertexArrayDescriptor(..),
    NumComponents, DataType(..), Stride)

{- TODO:

a few vaguely reasonable candidates for lensifying and/or prismatification

* boolean lenses for BufferMode (?)
* Evaluators.MapDescriptor
* Feedback.FeedbackToken, Feedback.VertexInfo, Feedback.ColorInfo
* Fog.FogMode
* FramebufferObjects:
    * FramebufferObjects.FramebufferObject
    * RenderbufferObjects.RenderbufferObject, RenderbufferObjects.Samples
* PixelData.PixelData
* Selection.HitRecord
* Texturing:
    * Environments.Arg
    * Specification.CompressedPixelData
* GLU.NURBS.SamplingMethod
* GLU.Quadrics.QuadricStyle, prism for GLU.Quadrics.QuadricPrimitive
* GLU.Tessellation.AnnotatedVertex, GLU.Tessellation.ComplexContour, 
  GLU.Tessellation.ComplexPolygon, GLU.Tessellation.WeightedProperties, 
  GLU.Tessellation.SimpleContour, GLU.Tessellation.PolygonContours, 
  GLU.Tessellation.Triangle, GLU.Tessellation.Triangulation, 
  GLU.Tessellation.Primitive, GLU.Tessellation.SimplePolygon
-}


swizzle2 :: (Field1 b b a1 a1, Field2 b b a2 a2) 
         => Lens' t a1 -> Lens' t a2 
         -> (a1 -> a2 -> a) 
         -> Lens t t a b
swizzle2 l1 l2 f = lens 
    (\x -> f (x^.l1) (x^.l2)) 
    (\x y -> (l1 .~ (y^._1)) . (l2 .~ (y^._2)) $ x)

swizzle3 :: (Field1 b b a1 a1, Field2 b b a2 a2, Field3 b b a3 a3) 
         => Lens' t a1 -> Lens' t a2 -> Lens' t a3 
         -> (a1 -> a2 -> a3 -> a) 
         -> Lens t t a b
swizzle3 l1 l2 l3 f = lens 
    (\x -> f (x^.l1) (x^.l2) (x^.l3)) 
    (\x y -> (l1 .~ (y^._1)) . (l2 .~ (y^._2)) . (l3 .~ (y^._3)) $ x)

swizzle4 :: (Field1 b b a1 a1, Field2 b b a2 a2, Field3 b b a3 a3, Field4 b b a4 a4) 
         => Lens' t a1 -> Lens' t a2 -> Lens' t a3 -> Lens' t a4
         -> (a1 -> a2 -> a3 -> a4 -> a) 
         -> Lens t t a b
swizzle4 l1 l2 l3 l4 f = lens 
    (\x -> f (x^.l1) (x^.l2) (x^.l3) (x^.l4)) 
    (\x y -> (l1 .~ (y^._1)) . (l2 .~ (y^._2)) . (l3 .~ (y^._3)) . (l4 .~ (y^._4)) $ x)


vaComponents :: Lens' (VertexArrayDescriptor a) NumComponents
vaComponents = lens 
    (\(VertexArrayDescriptor c _ _ _) -> c) 
    (\(VertexArrayDescriptor _ dt s ptr) c -> VertexArrayDescriptor c dt s ptr)

vaDataType :: Lens' (VertexArrayDescriptor a) DataType
vaDataType = lens 
    (\(VertexArrayDescriptor _ dt _ _) -> dt) 
    (\(VertexArrayDescriptor c _ s ptr) dt -> VertexArrayDescriptor c dt s ptr)

vaStride :: Lens' (VertexArrayDescriptor a) Stride
vaStride = lens 
    (\(VertexArrayDescriptor _ _ s _) -> s) 
    (\(VertexArrayDescriptor c dt _ ptr) s -> VertexArrayDescriptor c dt s ptr)

vaPtr :: Lens (VertexArrayDescriptor a) (VertexArrayDescriptor b) (Ptr a) (Ptr b)
vaPtr = lens 
    (\(VertexArrayDescriptor _ _ _ ptr) -> ptr) 
    (\(VertexArrayDescriptor c dt s _) ptr -> VertexArrayDescriptor c dt s ptr)



type instance Index (Vertex1 a) = Int
type instance Index (Vertex2 a) = Int
type instance Index (Vertex3 a) = Int
type instance Index (Vertex4 a) = Int

type instance IxValue (Vertex1 a) = a
type instance IxValue (Vertex2 a) = a
type instance IxValue (Vertex3 a) = a
type instance IxValue (Vertex4 a) = a

instance Field1 (Vertex1 a) (Vertex1 b) a b where 
  _1 f (Vertex1 a) = Vertex1 <$> indexed f (0 :: Int) a
  {-# INLINE _1 #-}

instance (a ~ a') => Field1 (Vertex2 a) (Vertex2 a') a a' where 
  _1 f (Vertex2 a b) = indexed f (0 :: Int) a <&> \a' -> Vertex2 a' b
  {-# INLINE _1 #-}

instance (a ~ a') => Field1 (Vertex3 a) (Vertex3 a') a a' where 
  _1 f (Vertex3 a b c) = indexed f (0 :: Int) a <&> \a' -> Vertex3 a' b c
  {-# INLINE _1 #-}

instance (a ~ a') => Field1 (Vertex4 a) (Vertex4 a') a a' where 
  _1 f (Vertex4 a b c d) = indexed f (0 :: Int) a <&> \a' -> Vertex4 a' b c d
  {-# INLINE _1 #-}

instance (a ~ a') => Field2 (Vertex2 a) (Vertex2 a') a a' where 
  _2 f (Vertex2 a b) = indexed f (1 :: Int) b <&> \b' -> Vertex2 a b'
  {-# INLINE _2 #-}

instance (a ~ a') => Field2 (Vertex3 a) (Vertex3 a') a a' where 
  _2 f (Vertex3 a b c) = indexed f (1 :: Int) b <&> \b' -> Vertex3 a b' c
  {-# INLINE _2 #-}

instance (a ~ a') => Field2 (Vertex4 a) (Vertex4 a') a a' where 
  _2 f (Vertex4 a b c d) = indexed f (1 :: Int) b <&> \b' -> Vertex4 a b' c d
  {-# INLINE _2 #-}

instance (a ~ a') => Field3 (Vertex3 a) (Vertex3 a') a a' where 
  _3 f (Vertex3 a b c) = indexed f (2 :: Int) c <&> \c' -> Vertex3 a b c'
  {-# INLINE _3 #-}

instance (a ~ a') => Field3 (Vertex4 a) (Vertex4 a') a a' where 
  _3 f (Vertex4 a b c d) = indexed f (2 :: Int) c <&> \c' -> Vertex4 a b c' d
  {-# INLINE _3 #-}

instance (a ~ a') => Field4 (Vertex4 a) (Vertex4 a') a a' where 
  _4 f (Vertex4 a b c d) = indexed f (3 :: Int) d <&> \d' -> Vertex4 a b c d'
  {-# INLINE _4 #-}

instance (Functor f) => Each f (Vertex1 a) (Vertex1 b) a b where
  each f (Vertex1 a) = Vertex1 <$> indexed f (0 :: Int) a
  {-# INLINE each #-}

instance (Applicative f) => Each f (Vertex2 a) (Vertex2 b) a b where
  each f (Vertex2 a b) = Vertex2 <$> f' (0 :: Int) a <*> f' 1 b
    where f' = indexed f
  {-# INLINE each #-}

instance (Applicative f) => Each f (Vertex3 a) (Vertex3 b) a b where
  each f (Vertex3 a b c) = Vertex3 <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c
    where f' = indexed f
  {-# INLINE each #-}

instance (Applicative f) => Each f (Vertex4 a) (Vertex4 b) a b where
  each f (Vertex4 a b c d) = Vertex4 <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c <*> f' 3 d
    where f' = indexed f
  {-# INLINE each #-}

instance Functor f => Ixed f (Vertex1 a) where
  ix _ f (Vertex1 a) = Vertex1 <$> indexed f (0 :: Int) a
  {-# INLINE ix #-}

instance (Applicative f) => Ixed f (Vertex2 a) where
  ix = ixEach
  {-# INLINE ix #-}

instance (Applicative f) => Ixed f (Vertex3 a) where
  ix = ixEach
  {-# INLINE ix #-}

instance (Applicative f) => Ixed f (Vertex4 a) where
  ix = ixEach
  {-# INLINE ix #-}

instance (Contravariant k, Functor k) => Contains k (Vertex1 a) where
  contains _ f _ = coerce (indexed f (0 :: Int) True)
  {-# INLINE contains #-}

instance (Contravariant k, Functor k) => Contains k (Vertex2 a) where
  contains = containsN 2
  {-# INLINE contains #-}

instance (Contravariant k, Functor k) => Contains k (Vertex3 a) where
  contains = containsN 3
  {-# INLINE contains #-}

instance (Contravariant k, Functor k) => Contains k (Vertex4 a) where
  contains = containsN 4
  {-# INLINE contains #-}



type instance Index (Vector1 a) = Int
type instance Index (Vector2 a) = Int
type instance Index (Vector3 a) = Int
type instance Index (Vector4 a) = Int

type instance IxValue (Vector1 a) = a
type instance IxValue (Vector2 a) = a
type instance IxValue (Vector3 a) = a
type instance IxValue (Vector4 a) = a

instance Field1 (Vector1 a) (Vector1 b) a b where 
  _1 f (Vector1 a) = Vector1 <$> indexed f (0 :: Int) a
  {-# INLINE _1 #-}

instance (a ~ a') => Field1 (Vector2 a) (Vector2 a') a a' where 
  _1 f (Vector2 a b) = indexed f (0 :: Int) a <&> \a' -> Vector2 a' b
  {-# INLINE _1 #-}

instance (a ~ a') => Field1 (Vector3 a) (Vector3 a') a a' where 
  _1 f (Vector3 a b c) = indexed f (0 :: Int) a <&> \a' -> Vector3 a' b c
  {-# INLINE _1 #-}

instance (a ~ a') => Field1 (Vector4 a) (Vector4 a') a a' where 
  _1 f (Vector4 a b c d) = indexed f (0 :: Int) a <&> \a' -> Vector4 a' b c d
  {-# INLINE _1 #-}

instance (a ~ a') => Field2 (Vector2 a) (Vector2 a') a a' where 
  _2 f (Vector2 a b) = indexed f (1 :: Int) b <&> \b' -> Vector2 a b'
  {-# INLINE _2 #-}

instance (a ~ a') => Field2 (Vector3 a) (Vector3 a') a a' where 
  _2 f (Vector3 a b c) = indexed f (1 :: Int) b <&> \b' -> Vector3 a b' c
  {-# INLINE _2 #-}

instance (a ~ a') => Field2 (Vector4 a) (Vector4 a') a a' where 
  _2 f (Vector4 a b c d) = indexed f (1 :: Int) b <&> \b' -> Vector4 a b' c d
  {-# INLINE _2 #-}

instance (a ~ a') => Field3 (Vector3 a) (Vector3 a') a a' where 
  _3 f (Vector3 a b c) = indexed f (2 :: Int) c <&> \c' -> Vector3 a b c'
  {-# INLINE _3 #-}

instance (a ~ a') => Field3 (Vector4 a) (Vector4 a') a a' where 
  _3 f (Vector4 a b c d) = indexed f (2 :: Int) c <&> \c' -> Vector4 a b c' d
  {-# INLINE _3 #-}

instance (a ~ a') => Field4 (Vector4 a) (Vector4 a') a a' where 
  _4 f (Vector4 a b c d) = indexed f (3 :: Int) d <&> \d' -> Vector4 a b c d'
  {-# INLINE _4 #-}

instance (Functor f) => Each f (Vector1 a) (Vector1 b) a b where
  each f (Vector1 a) = Vector1 <$> indexed f (0 :: Int) a
  {-# INLINE each #-}

instance (Applicative f) => Each f (Vector2 a) (Vector2 b) a b where
  each f (Vector2 a b) = Vector2 <$> f' (0 :: Int) a <*> f' 1 b
    where f' = indexed f
  {-# INLINE each #-}

instance (Applicative f) => Each f (Vector3 a) (Vector3 b) a b where
  each f (Vector3 a b c) = Vector3 <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c
    where f' = indexed f
  {-# INLINE each #-}

instance (Applicative f) => Each f (Vector4 a) (Vector4 b) a b where
  each f (Vector4 a b c d) = Vector4 <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c <*> f' 3 d
    where f' = indexed f
  {-# INLINE each #-}

instance Functor f => Ixed f (Vector1 a) where
  ix _ f (Vector1 a) = Vector1 <$> indexed f (0 :: Int) a
  {-# INLINE ix #-}

instance (Applicative f) => Ixed f (Vector2 a) where
  ix = ixEach
  {-# INLINE ix #-}

instance (Applicative f) => Ixed f (Vector3 a) where
  ix = ixEach
  {-# INLINE ix #-}

instance (Applicative f) => Ixed f (Vector4 a) where
  ix = ixEach
  {-# INLINE ix #-}

instance (Contravariant k, Functor k) => Contains k (Vector1 a) where
  contains _ f _ = coerce (indexed f (0 :: Int) True)
  {-# INLINE contains #-}

instance (Contravariant k, Functor k) => Contains k (Vector2 a) where
  contains = containsN 2
  {-# INLINE contains #-}

instance (Contravariant k, Functor k) => Contains k (Vector3 a) where
  contains = containsN 3
  {-# INLINE contains #-}

instance (Contravariant k, Functor k) => Contains k (Vector4 a) where
  contains = containsN 4
  {-# INLINE contains #-}




type instance Index (TexCoord1 a) = Int
type instance Index (TexCoord2 a) = Int
type instance Index (TexCoord3 a) = Int
type instance Index (TexCoord4 a) = Int

type instance IxValue (TexCoord1 a) = a
type instance IxValue (TexCoord2 a) = a
type instance IxValue (TexCoord3 a) = a
type instance IxValue (TexCoord4 a) = a

instance Field1 (TexCoord1 a) (TexCoord1 b) a b where 
  _1 f (TexCoord1 a) = TexCoord1 <$> indexed f (0 :: Int) a
  {-# INLINE _1 #-}

instance (a ~ a') => Field1 (TexCoord2 a) (TexCoord2 a') a a' where 
  _1 f (TexCoord2 a b) = indexed f (0 :: Int) a <&> \a' -> TexCoord2 a' b
  {-# INLINE _1 #-}

instance (a ~ a') => Field1 (TexCoord3 a) (TexCoord3 a') a a' where 
  _1 f (TexCoord3 a b c) = indexed f (0 :: Int) a <&> \a' -> TexCoord3 a' b c
  {-# INLINE _1 #-}

instance (a ~ a') => Field1 (TexCoord4 a) (TexCoord4 a') a a' where 
  _1 f (TexCoord4 a b c d) = indexed f (0 :: Int) a <&> \a' -> TexCoord4 a' b c d
  {-# INLINE _1 #-}

instance (a ~ a') => Field2 (TexCoord2 a) (TexCoord2 a') a a' where 
  _2 f (TexCoord2 a b) = indexed f (1 :: Int) b <&> \b' -> TexCoord2 a b'
  {-# INLINE _2 #-}

instance (a ~ a') => Field2 (TexCoord3 a) (TexCoord3 a') a a' where 
  _2 f (TexCoord3 a b c) = indexed f (1 :: Int) b <&> \b' -> TexCoord3 a b' c
  {-# INLINE _2 #-}

instance (a ~ a') => Field2 (TexCoord4 a) (TexCoord4 a') a a' where 
  _2 f (TexCoord4 a b c d) = indexed f (1 :: Int) b <&> \b' -> TexCoord4 a b' c d
  {-# INLINE _2 #-}

instance (a ~ a') => Field3 (TexCoord3 a) (TexCoord3 a') a a' where 
  _3 f (TexCoord3 a b c) = indexed f (2 :: Int) c <&> \c' -> TexCoord3 a b c'
  {-# INLINE _3 #-}

instance (a ~ a') => Field3 (TexCoord4 a) (TexCoord4 a') a a' where 
  _3 f (TexCoord4 a b c d) = indexed f (2 :: Int) c <&> \c' -> TexCoord4 a b c' d
  {-# INLINE _3 #-}

instance (a ~ a') => Field4 (TexCoord4 a) (TexCoord4 a') a a' where 
  _4 f (TexCoord4 a b c d) = indexed f (3 :: Int) d <&> \d' -> TexCoord4 a b c d'
  {-# INLINE _4 #-}

instance (Functor f) => Each f (TexCoord1 a) (TexCoord1 b) a b where
  each f (TexCoord1 a) = TexCoord1 <$> indexed f (0 :: Int) a
  {-# INLINE each #-}

instance (Applicative f) => Each f (TexCoord2 a) (TexCoord2 b) a b where
  each f (TexCoord2 a b) = TexCoord2 <$> f' (0 :: Int) a <*> f' 1 b
    where f' = indexed f
  {-# INLINE each #-}

instance (Applicative f) => Each f (TexCoord3 a) (TexCoord3 b) a b where
  each f (TexCoord3 a b c) = TexCoord3 <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c
    where f' = indexed f
  {-# INLINE each #-}

instance (Applicative f) => Each f (TexCoord4 a) (TexCoord4 b) a b where
  each f (TexCoord4 a b c d) = TexCoord4 <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c <*> f' 3 d
    where f' = indexed f
  {-# INLINE each #-}

instance Functor f => Ixed f (TexCoord1 a) where
  ix _ f (TexCoord1 a) = TexCoord1 <$> indexed f (0 :: Int) a
  {-# INLINE ix #-}

instance (Applicative f) => Ixed f (TexCoord2 a) where
  ix = ixEach
  {-# INLINE ix #-}

instance (Applicative f) => Ixed f (TexCoord3 a) where
  ix = ixEach
  {-# INLINE ix #-}

instance (Applicative f) => Ixed f (TexCoord4 a) where
  ix = ixEach
  {-# INLINE ix #-}

instance (Contravariant k, Functor k) => Contains k (TexCoord1 a) where
  contains _ f _ = coerce (indexed f (0 :: Int) True)
  {-# INLINE contains #-}

instance (Contravariant k, Functor k) => Contains k (TexCoord2 a) where
  contains = containsN 2
  {-# INLINE contains #-}

instance (Contravariant k, Functor k) => Contains k (TexCoord3 a) where
  contains = containsN 3
  {-# INLINE contains #-}

instance (Contravariant k, Functor k) => Contains k (TexCoord4 a) where
  contains = containsN 4
  {-# INLINE contains #-}



type instance Index (FogCoord1 a) = Int

type instance IxValue (FogCoord1 a) = a

instance Field1 (FogCoord1 a) (FogCoord1 b) a b where 
  _1 f (FogCoord1 a) = FogCoord1 <$> indexed f (0 :: Int) a
  {-# INLINE _1 #-}

instance (Functor f) => Each f (FogCoord1 a) (FogCoord1 b) a b where
  each f (FogCoord1 a) = FogCoord1 <$> indexed f (0 :: Int) a
  {-# INLINE each #-}

instance Functor f => Ixed f (FogCoord1 a) where
  ix _ f (FogCoord1 a) = FogCoord1 <$> indexed f (0 :: Int) a
  {-# INLINE ix #-}

instance (Contravariant k, Functor k) => Contains k (FogCoord1 a) where
  contains _ f _ = coerce (indexed f (0 :: Int) True)
  {-# INLINE contains #-}




type instance Index (Color3 a) = Int
type instance Index (Color4 a) = Int

type instance IxValue (Color3 a) = a
type instance IxValue (Color4 a) = a

instance (a ~ a') => Field1 (Color3 a) (Color3 a') a a' where 
  _1 f (Color3 a b c) = indexed f (0 :: Int) a <&> \a' -> Color3 a' b c
  {-# INLINE _1 #-}

instance (a ~ a') => Field1 (Color4 a) (Color4 a') a a' where 
  _1 f (Color4 a b c d) = indexed f (0 :: Int) a <&> \a' -> Color4 a' b c d
  {-# INLINE _1 #-}

instance (a ~ a') => Field2 (Color3 a) (Color3 a') a a' where 
  _2 f (Color3 a b c) = indexed f (1 :: Int) b <&> \b' -> Color3 a b' c
  {-# INLINE _2 #-}

instance (a ~ a') => Field2 (Color4 a) (Color4 a') a a' where 
  _2 f (Color4 a b c d) = indexed f (1 :: Int) b <&> \b' -> Color4 a b' c d
  {-# INLINE _2 #-}

instance (a ~ a') => Field3 (Color3 a) (Color3 a') a a' where 
  _3 f (Color3 a b c) = indexed f (2 :: Int) c <&> \c' -> Color3 a b c'
  {-# INLINE _3 #-}

instance (a ~ a') => Field3 (Color4 a) (Color4 a') a a' where 
  _3 f (Color4 a b c d) = indexed f (2 :: Int) c <&> \c' -> Color4 a b c' d
  {-# INLINE _3 #-}

instance (a ~ a') => Field4 (Color4 a) (Color4 a') a a' where 
  _4 f (Color4 a b c d) = indexed f (3 :: Int) d <&> \d' -> Color4 a b c d'
  {-# INLINE _4 #-}

instance (Applicative f) => Each f (Color3 a) (Color3 b) a b where
  each f (Color3 a b c) = Color3 <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c
    where f' = indexed f
  {-# INLINE each #-}

instance (Applicative f) => Each f (Color4 a) (Color4 b) a b where
  each f (Color4 a b c d) = Color4 <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c <*> f' 3 d
    where f' = indexed f
  {-# INLINE each #-}

instance (Applicative f) => Ixed f (Color3 a) where
  ix = ixEach
  {-# INLINE ix #-}

instance (Applicative f) => Ixed f (Color4 a) where
  ix = ixEach
  {-# INLINE ix #-}

instance (Contravariant k, Functor k) => Contains k (Color3 a) where
  contains = containsN 3
  {-# INLINE contains #-}

instance (Contravariant k, Functor k) => Contains k (Color4 a) where
  contains = containsN 4
  {-# INLINE contains #-}


type instance Index (Normal3 a) = Int

type instance IxValue (Normal3 a) = a

instance (a ~ a') => Field1 (Normal3 a) (Normal3 a') a a' where 
  _1 f (Normal3 a b c) = indexed f (0 :: Int) a <&> \a' -> Normal3 a' b c
  {-# INLINE _1 #-}

instance (a ~ a') => Field2 (Normal3 a) (Normal3 a') a a' where 
  _2 f (Normal3 a b c) = indexed f (1 :: Int) b <&> \b' -> Normal3 a b' c
  {-# INLINE _2 #-}

instance (a ~ a') => Field3 (Normal3 a) (Normal3 a') a a' where 
  _3 f (Normal3 a b c) = indexed f (2 :: Int) c <&> \c' -> Normal3 a b c'
  {-# INLINE _3 #-}

instance (Applicative f) => Each f (Normal3 a) (Normal3 b) a b where
  each f (Normal3 a b c) = Normal3 <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c
    where f' = indexed f
  {-# INLINE each #-}

instance (Applicative f) => Ixed f (Normal3 a) where
  ix = ixEach
  {-# INLINE ix #-}

instance (Contravariant k, Functor k) => Contains k (Normal3 a) where
  contains = containsN 3
  {-# INLINE contains #-}



type instance Index (Index1 a) = Int

type instance IxValue (Index1 a) = a

instance Field1 (Index1 a) (Index1 b) a b where 
  _1 f (Index1 a) = Index1 <$> indexed f (0 :: Int) a
  {-# INLINE _1 #-}

instance (Functor f) => Each f (Index1 a) (Index1 b) a b where
  each f (Index1 a) = Index1 <$> indexed f (0 :: Int) a
  {-# INLINE each #-}

instance Functor f => Ixed f (Index1 a) where
  ix _ f (Index1 a) = Index1 <$> indexed f (0 :: Int) a
  {-# INLINE ix #-}

instance (Contravariant k, Functor k) => Contains k (Index1 a) where
  contains _ f _ = coerce (indexed f (0 :: Int) True)
  {-# INLINE contains #-}




type instance Index Position = Int
type instance IxValue Position = GLint

instance Field1 Position Position GLint GLint where 
  _1 f (Position a b) = indexed f (0 :: Int) a <&> \a' -> Position a' b
  {-# INLINE _1 #-}

instance Field2 Position Position GLint GLint where 
  _2 f (Position a b) = indexed f (1 :: Int) b <&> \b' -> Position a b'
  {-# INLINE _2 #-}

instance (Applicative f) => Each f Position Position GLint GLint where
  each f (Position a b) = Position <$> f' (0 :: Int) a <*> f' 1 b
    where f' = indexed f
  {-# INLINE each #-}

instance (Applicative f) => Ixed f Position where
  ix = ixEach
  {-# INLINE ix #-}

instance (Contravariant k, Functor k) => Contains k Position where
  contains = containsN 2
  {-# INLINE contains #-}


type instance Index Size = Int
type instance IxValue Size = GLsizei

instance Field1 Size Size GLsizei GLsizei where 
  _1 f (Size a b) = indexed f (0 :: Int) a <&> \a' -> Size a' b
  {-# INLINE _1 #-}

instance Field2 Size Size GLsizei GLsizei where 
  _2 f (Size a b) = indexed f (1 :: Int) b <&> \b' -> Size a b'
  {-# INLINE _2 #-}

instance (Applicative f) => Each f Size Size GLsizei GLsizei where
  each f (Size a b) = Size <$> f' (0 :: Int) a <*> f' 1 b
    where f' = indexed f
  {-# INLINE each #-}

instance (Applicative f) => Ixed f Size where
  ix = ixEach
  {-# INLINE ix #-}

instance (Contravariant k, Functor k) => Contains k Size where
  contains = containsN 2
  {-# INLINE contains #-}



type instance Index (Plane a) = Int
type instance IxValue (Plane a) = a

instance (a ~ a') => Field1 (Plane a) (Plane a') a a' where 
  _1 f (Plane a b c d) = indexed f (0 :: Int) a <&> \a' -> Plane a' b c d
  {-# INLINE _1 #-}

instance (a ~ a') => Field2 (Plane a) (Plane a') a a' where 
  _2 f (Plane a b c d) = indexed f (1 :: Int) b <&> \b' -> Plane a b' c d
  {-# INLINE _2 #-}

instance (a ~ a') => Field3 (Plane a) (Plane a') a a' where 
  _3 f (Plane a b c d) = indexed f (2 :: Int) c <&> \c' -> Plane a b c' d
  {-# INLINE _3 #-}

instance (a ~ a') => Field4 (Plane a) (Plane a') a a' where 
  _4 f (Plane a b c d) = indexed f (3 :: Int) d <&> \d' -> Plane a b c d'
  {-# INLINE _4 #-}

instance (Applicative f) => Each f (Plane a) (Plane b) a b where
  each f (Plane a b c d) = Plane <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c <*> f' 3 d
    where f' = indexed f
  {-# INLINE each #-}

instance (Applicative f) => Ixed f (Plane a) where
  ix = ixEach
  {-# INLINE ix #-}

instance (Contravariant k, Functor k) => Contains k (Plane a) where
  contains = containsN 4
  {-# INLINE contains #-}



type instance Index RenderbufferSize = Int
type instance IxValue RenderbufferSize = GLsizei

instance Field1 RenderbufferSize RenderbufferSize GLsizei GLsizei where 
  _1 f (RenderbufferSize a b) = indexed f (0 :: Int) a <&> \a' -> RenderbufferSize a' b
  {-# INLINE _1 #-}

instance Field2 RenderbufferSize RenderbufferSize GLsizei GLsizei where 
  _2 f (RenderbufferSize a b) = indexed f (1 :: Int) b <&> \b' -> RenderbufferSize a b'
  {-# INLINE _2 #-}

instance (Applicative f) => Each f RenderbufferSize RenderbufferSize GLsizei GLsizei where
  each f (RenderbufferSize a b) = RenderbufferSize <$> f' (0 :: Int) a <*> f' 1 b
    where f' = indexed f
  {-# INLINE each #-}

instance (Applicative f) => Ixed f RenderbufferSize where
  ix = ixEach
  {-# INLINE ix #-}

instance (Contravariant k, Functor k) => Contains k RenderbufferSize where
  contains = containsN 2
  {-# INLINE contains #-}


type instance Index TexturePosition1D = Int
type instance Index TexturePosition2D = Int
type instance Index TexturePosition3D = Int

type instance IxValue TexturePosition1D = GLint
type instance IxValue TexturePosition2D = GLint
type instance IxValue TexturePosition3D = GLint

instance Field1 TexturePosition1D TexturePosition1D GLint GLint where 
  _1 f (TexturePosition1D a) = TexturePosition1D <$> indexed f (0 :: Int) a
  {-# INLINE _1 #-}

instance Field1 TexturePosition2D TexturePosition2D GLint GLint where 
  _1 f (TexturePosition2D a b) = indexed f (0 :: Int) a <&> \a' -> TexturePosition2D a' b
  {-# INLINE _1 #-}

instance Field1 TexturePosition3D TexturePosition3D GLint GLint where 
  _1 f (TexturePosition3D a b c) = indexed f (0 :: Int) a <&> \a' -> TexturePosition3D a' b c
  {-# INLINE _1 #-}

instance Field2 TexturePosition2D TexturePosition2D GLint GLint where 
  _2 f (TexturePosition2D a b) = indexed f (1 :: Int) b <&> \b' -> TexturePosition2D a b'
  {-# INLINE _2 #-}

instance Field2 TexturePosition3D TexturePosition3D GLint GLint where 
  _2 f (TexturePosition3D a b c) = indexed f (1 :: Int) b <&> \b' -> TexturePosition3D a b' c
  {-# INLINE _2 #-}

instance Field3 TexturePosition3D TexturePosition3D GLint GLint where 
  _3 f (TexturePosition3D a b c) = indexed f (2 :: Int) c <&> \c' -> TexturePosition3D a b c'
  {-# INLINE _3 #-}

instance (Functor f) => Each f TexturePosition1D TexturePosition1D GLint GLint where
  each f (TexturePosition1D a) = TexturePosition1D <$> indexed f (0 :: Int) a
  {-# INLINE each #-}

instance (Applicative f) => Each f TexturePosition2D TexturePosition2D GLint GLint where
  each f (TexturePosition2D a b) = TexturePosition2D <$> f' (0 :: Int) a <*> f' 1 b
    where f' = indexed f
  {-# INLINE each #-}

instance (Applicative f) => Each f TexturePosition3D TexturePosition3D GLint GLint where
  each f (TexturePosition3D a b c) = TexturePosition3D <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c
    where f' = indexed f
  {-# INLINE each #-}

instance Functor f => Ixed f TexturePosition1D where
  ix _ f (TexturePosition1D a) = TexturePosition1D <$> indexed f (0 :: Int) a
  {-# INLINE ix #-}

instance (Applicative f) => Ixed f TexturePosition2D where
  ix = ixEach
  {-# INLINE ix #-}

instance (Applicative f) => Ixed f TexturePosition3D where
  ix = ixEach
  {-# INLINE ix #-}

instance (Contravariant k, Functor k) => Contains k TexturePosition1D where
  contains _ f _ = coerce (indexed f (0 :: Int) True)
  {-# INLINE contains #-}

instance (Contravariant k, Functor k) => Contains k TexturePosition2D where
  contains = containsN 2
  {-# INLINE contains #-}

instance (Contravariant k, Functor k) => Contains k TexturePosition3D where
  contains = containsN 3
  {-# INLINE contains #-}


type instance Index TextureSize1D = Int
type instance Index TextureSize2D = Int
type instance Index TextureSize3D = Int

type instance IxValue TextureSize1D = GLsizei
type instance IxValue TextureSize2D = GLsizei
type instance IxValue TextureSize3D = GLsizei

instance Field1 TextureSize1D TextureSize1D GLsizei GLsizei where 
  _1 f (TextureSize1D a) = TextureSize1D <$> indexed f (0 :: Int) a
  {-# INLINE _1 #-}

instance Field1 TextureSize2D TextureSize2D GLsizei GLsizei where 
  _1 f (TextureSize2D a b) = indexed f (0 :: Int) a <&> \a' -> TextureSize2D a' b
  {-# INLINE _1 #-}

instance Field1 TextureSize3D TextureSize3D GLsizei GLsizei where 
  _1 f (TextureSize3D a b c) = indexed f (0 :: Int) a <&> \a' -> TextureSize3D a' b c
  {-# INLINE _1 #-}

instance Field2 TextureSize2D TextureSize2D GLsizei GLsizei where 
  _2 f (TextureSize2D a b) = indexed f (1 :: Int) b <&> \b' -> TextureSize2D a b'
  {-# INLINE _2 #-}

instance Field2 TextureSize3D TextureSize3D GLsizei GLsizei where 
  _2 f (TextureSize3D a b c) = indexed f (1 :: Int) b <&> \b' -> TextureSize3D a b' c
  {-# INLINE _2 #-}

instance Field3 TextureSize3D TextureSize3D GLsizei GLsizei where 
  _3 f (TextureSize3D a b c) = indexed f (2 :: Int) c <&> \c' -> TextureSize3D a b c'
  {-# INLINE _3 #-}

instance (Functor f) => Each f TextureSize1D TextureSize1D GLsizei GLsizei where
  each f (TextureSize1D a) = TextureSize1D <$> indexed f (0 :: Int) a
  {-# INLINE each #-}

instance (Applicative f) => Each f TextureSize2D TextureSize2D GLsizei GLsizei where
  each f (TextureSize2D a b) = TextureSize2D <$> f' (0 :: Int) a <*> f' 1 b
    where f' = indexed f
  {-# INLINE each #-}

instance (Applicative f) => Each f TextureSize3D TextureSize3D GLsizei GLsizei where
  each f (TextureSize3D a b c) = TextureSize3D <$> f' (0 :: Int) a <*> f' 1 b <*> f' 2 c
    where f' = indexed f
  {-# INLINE each #-}

instance Functor f => Ixed f TextureSize1D where
  ix _ f (TextureSize1D a) = TextureSize1D <$> indexed f (0 :: Int) a
  {-# INLINE ix #-}

instance (Applicative f) => Ixed f TextureSize2D where
  ix = ixEach
  {-# INLINE ix #-}

instance (Applicative f) => Ixed f TextureSize3D where
  ix = ixEach
  {-# INLINE ix #-}

instance (Contravariant k, Functor k) => Contains k TextureSize1D where
  contains _ f _ = coerce (indexed f (0 :: Int) True)
  {-# INLINE contains #-}

instance (Contravariant k, Functor k) => Contains k TextureSize2D where
  contains = containsN 2
  {-# INLINE contains #-}

instance (Contravariant k, Functor k) => Contains k TextureSize3D where
  contains = containsN 3
  {-# INLINE contains #-}


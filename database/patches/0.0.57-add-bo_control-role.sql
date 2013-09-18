update usermetatbl set roles = roles || array['bo_control'] where 'back' = any(roles);
